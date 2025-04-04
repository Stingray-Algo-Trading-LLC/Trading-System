module Algos.LVRH
  ( lvrhBuyLogic,
    lvrhStateTransition,
    LVRHState,
    initLVRH
  )
where

import Data.Ord (Ord (max, min))
import Lib.DataTypes (StreamData (..))
import Lib.Utils (getSaleCondition, utcToUnixSeconds, safeInit)
import Lib.Hammer (isGreenHammer, isHammerInResDeflectZone)
import Lib.LevelsAccel (initResistanceLevelsAcc)
import Lib.Levels (resistanceLevels)
import Data.List (sort)

data OhlcState
  = OhlcState
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    closePrice :: Double -- Last qualifying trade price. This algo only assumes trades with condition code 1.
  }
  deriving (Show)

data TimeState
  = TimeState
  { openTime :: Double, -- The Unix timestamp of the last trade to update openPrice.
    firstTime :: Double, -- The Unix timestamp of the earliest trade received, condition code is irrelevant.
    lastTime :: Double -- The Unix timestamp of the last trade to update closePrice.
  }
  deriving (Show)

data LevelState
  = LevelState
  { barTops :: [Double], -- Sequence of bar top prices. Open/Close price for Red/Green bars.
    barHighs :: [Double], -- Sequence of bar high prices.
    resLevels :: [Double] -- Resistance price levels.
  }
  deriving (Show)

data OrderState
  = OrderState
  { buyLock :: Bool,
    buyQty :: Int,
    genOrders :: OrderStrategy
  }
data ConstantState
  = ConstantState
  { genResLevels :: [Double] -> [Double] -> [Double],
    hasDelayTimeElapsed :: Double -> Double -> Bool,
    isHammer :: Double -> Double -> Double -> Double -> Bool,
    inDeflectZone :: Double -> Double -> [Double] -> (Bool, Double) 
  }

newtype AssetStateTransition = AssetStateTransition (StreamData -> (OhlcState, LevelState, TimeState, AssetStateTransition))
assetStateMachine :: ConstantState -> OhlcState -> LevelState -> TimeState -> AssetStateTransition
assetStateMachine constSt initOhlcSt initLevelSt initTimeSt =
  AssetStateTransition $ assetStateTrans initOhlcSt initLevelSt initTimeSt
  where 
    assetStateTrans :: OhlcState -> LevelState -> TimeState -> StreamData -> (OhlcState, LevelState, TimeState, AssetStateTransition)
    assetStateTrans ohlcSt levelSt timeSt (TradeData trade)
      | saleCondition == 0 = 
          let newTimeSt = timeSt {firstTime = min (firstTime timeSt) currTradeTime}
          in (ohlcSt, levelSt, newTimeSt, assetStateTrans ohlcSt levelSt newTimeSt)
      | otherwise = 
          let newOhlcSt = 
            ohlcSt
              { openPrice = if currTradeTime < openTime timeSt then price trade else openPrice ohlcSt,
                highPrice = max (highPrice ohlcSt) (price trade),
                lowPrice = min (lowPrice ohlcSt) (price trade),
                closePrice = if (saleCondition == 1) && (currTradeTime >= lastTime timeSt) then price trade else closePrice ohlcSt
              }
              newTimeSt =
            timeSt
              { openTime = min (openTime timeSt) currTradeTime,
                firstTime = min (firstTime timeSt) currTradeTime,
                lastTime = if saleCondition == 1 then max (lastTime timeSt) currTradeTime else lastTime timeSt
              }
          in (newOhlcSt, levelSt, newTimeSt, assetStateTrans newOhlcSt levelSt newTimeSt)
      where 
        saleCondition = getSaleCondition $ condition trade
        currTradeTime = utcToUnixSeconds $ tradeTimestamp trade
    assetStateTrans ohlcSt levelSt timeSt (BarData bar) = (newOhlcSt, newLevelSt, newTimeSt, assetStateTrans newOhlcSt newLevelSt newTimeSt)
      where
        newOhlcSt = ohlcSt
          { openPrice = 1/0, -- infinity
            highPrice = -(1/0), -- -infinity
            lowPrice = 1/0, -- infinity
            closePrice = 0.0
          }
        newTimeSt = timeSt
          { openTime = 1/0, -- infinity
            firstTime = 1/0, -- infinity
            lastTime = -(1/0) -- -infinity
          }
        newLevelSt = levelSt
          { barTops = newBarTops,
            barHighs = newBarHighs,
            resLevels = newResLevels -- getResLevels newBarTops newBarHighs
          }
        newBarTops = barTops levelSt ++ [max (open bar) (close bar)]
        newBarHighs = barHighs levelSt ++ [high bar]
        newResLevels = sort $ (genResLevels constSt) newBarHighs newBarHighs
    assetStateTrans ohlcSt levelSt timeSt (BarUpdateData bar) = (ohlcSt, newLevelSt, timeSt, assetStateTrans ohlcSt newLevelSt timeSt)
      where
        newLevelSt = levelSt
          { barTops = newBarTops,
            barHighs = newBarHighs,
            resLevels = newResLevels -- getResLevels newBarTops newBarHighs
          }
        newBarTops = safeInit (barTops levelSt) ++ [max (open bar) (close bar)]
        newBarHighs = safeInit (barHighs levelSt) ++ [high bar]
        newResLevels = sort $ (genResLevels constSt) newBarHighs newBarHighs

newtype OrderStateTransition = OrderStateTransition (OhlcState -> LevelState -> TimeState -> StreamData -> ([Order], OrderStateTransition))
orderStateMachine :: ConstantState -> OrderState -> OrderStateTransition
orderStateMachine constSt initOrderSt =
  OrderStateTransition $ orderStateTrans initOrderSt
  where
    orderStateTrans :: OrderState -> OhlcState -> LevelState -> TimeState -> StreamData -> ([Order], OrderStateTransition)
    orderStateTrans orderSt ohlcSt levelSt timeSt (BarData _) = ([], orderStateTrans $ orderSt {buyLock = False})
    orderStateTrans orderSt ohlcSt levelSt timeSt (BarUpdateData _) = ([], orderStateTrans $ orderSt)
    orderStateTrans orderSt ohlcSt levelSt timeSt (TradeData trade) = 
      (orders, orderStateTrans $ orderSt {buyLock = (buyLock orderSt) || shouldBuy, genOrders = newGenOrders })
      where
        (orders, newGenOrders) =
          case genOrders orderSt of
            TrailingOptionOrder f -> f (TradeData trade) qty resLevel (0.1 + closePrice ohlcSt)
        (isDeflec, resLevel) = 
          inDeflectZone constSt 
            (lowPrice ohlcSt) (closePrice ohlcSt) (resLevels levelSt)  -- Wick "touches" or is near resistance level.
        shouldBuy =
          not (buyLock orderSt) &&
          hasDelayTimeElapsed constSt 
            (firstTime timeSt) (lastTime timeSt) && -- Wait time to ensure open price is accurate.
          isHammer constSt 
            (openPrice ohlcSt) (highPrice ohlcSt) (lowPrice ohlcSt) (closePrice ohlcSt) &&
          isDeflec
        qty = (fromEnum shouldBuy) * (buyQty orderSt)

type Order = Bool
data OrderOutput = O1 Order | O2 (Order, Int -> OrderOutput) | O3 (Int -> OrderOutput)

trailingOptionOrder :: [OpenOrder] -> StreamData -> Int -> Double -> Double -> ([Order], OrderStrategy)
trailingOptionOrder openOrders streamData buyQty initStopLoss initTakeProf
  | noOpenOrders && noBuy = ([], nextStrategy [])
  | noOpenOrders && haveBuy = ([marketBuy], nextStrategy [newOpenOrder])
  | haveOpenOrders && noBuy = 
    let (orders, newOpenOrders) = queryOpenOrders ([],[]) openOrders price
    in (orders, nextStrategy newOpenOrders)
  | haveOpenOrders && haveBuy =
    let initOrderTup = ([marketBuy], [newOpenOrder])
        (orders, newOpenOrders) = queryOpenOrders initOrderTup openOrders price
    in (orders, nextStrategy newOpenOrders)
  where
    (undSymb, price, timestamp) = extractSymPriceTs streamData
    haveBuy = buyQty > 0
    noBuy = buyQty == 0
    haveOpenOrders = not $ null openOrders
    noOpenOrders = null openOrders
    optSymbol = genOtmOptionSymb undSymb timestamp price Call
    marketBuy = genMarketBuy optSymbol buyQty
    newOpenOrder = f optSymbol buyQty initStopLoss initTakeProf
    nextStrategy = OrderStrategy . trailingOptionOrder

extractBarSymPriceTs :: Bar -> (String, Double, UTCTime)
extractBarSymPriceTs b = (barSymbol b, close b, barTimestamp b)

extractSymPriceTs :: StreamData -> (String, Double, UTCTime)
extractSymPriceTs (TradeData t) = (tradeSymbol t, price t, tradeTimestamp t)
extractSymPriceTs (BarData b) = extractBarSymPriceTs b
extractSymPriceTs (BarUpdateData b) = extractBarSymPriceTs b

{-# LANGUAGE OverloadedStrings #-}
import Data.Time (UTCTime, toGregorian, utctDay)
import Data.Text (unpack)
import Text.Printf (printf)

data OptionType = Call | Put 
instance Show OptionType where
  show Call = "C" 
  show Put = "P"

parseYMD :: UTCTime -> (Integer, Int, Int)
parseYMD t = 
  let (yyyy, mm, dd) = toGregorian $ utctDay t
  in (yyyy, fromIntegral mm, fromIntegral dd)

roundStrike :: OptionType -> Double -> Int
roundStrike Call p = ceiling p
roundStrike Put p = floor p

formatOption :: String -> String -> (Integer, Int, Int) -> Int -> String
formatOption undSymb optTypeStr (yyyy, mm, dd) strike =
  let yyStr = printf "%02d" (yyyy `mod` 100)
      mmStr = printf "%02d" mm
      ddStr = printf "%O2d" dd
      strkStr = printf "%08d" (strike * 1000)
  in undSymb ++ yyStr ++ mmStr ++ ddStr ++ optTypeStr ++ strkStr

genOtmOptionSymb :: String -> UTCTime -> Double -> OptionType -> String
genOtmOptionSymb undSymb timestamp currPrice optType = 
  let ymd = parseYMD timestamp
      strike = roundStrike optType currPrice
  in formatOption undSymb (show optType) ymd strike


instance Show OrderOutput where
  show (O1 o) = "O1 " ++ show o
  show (O2 (o, _)) = "O2 (" ++ show o ++ ", <function>)"
  show (O3 _) = "O3 <function>"

trailingTakeProfitStopLossState :: Double -> Double -> Double -> OrderOutput
trailingTakeProfitStopLossState sl tk p -- stop-loss, take-profit, price
  | p >= tk = O2 (True, trailingTakeProfitStopLossState (tk+10) (sl+10))
  | p <= sl = O1 (False)
  | otherwise = O3 (trailingTakeProfitStopLossState tk sl)

data OpenOrder = OpenOrder (Double -> OrderOutput)
data OrderOutput = O1 Order | O2 (Order, OpenOrder) | O3 OpenOrder

genTrailStpLsTkPrfOrd :: String -> Int -> Double -> Double -> Double -> OrderOutput
genTrailStpLsTkPrfOrd symb qty sl tk p 
  | shouldSellRemains = O1 $ genMarketSell symb qty
  | shouldSellOne = O2 (genMarketSell symb 1, genTrailStpLsTkPrfOrd symb (qty-1) (sl+0.1) (tk+0.1))
  | otherwise = O3 $ genTrailStpLsTkPrfOrd symb qty sl tk
  where
    tkPrfTrigg = p >= tk 
    tkPrfWthOneShrTrigg = tkPrfTrigg && (qty == 1)
    stpLsTrigg = p <= sl
    shouldSellRemains = tkPrfWthOneShrTrigg || stpLsTrigg
    shouldSellOne = tkPrfTrigg && (qty > 1)

marketBuyOrder
func :: Int -> [OrderOutput] -> ([OrderOutput], [OrderOutput]) -> ([OrderOutput], [OrderOutput]) 
func _ [] oldTup = oldTup
func i (O3 f:fs) (o1Arr, o3Arr) = func i fs newTup
  where
    newTup = case f i of
      O1 o1 -> (o1Arr ++ [O1 o1], o3Arr)
      O2 (b, fx) -> (o1Arr ++ [O1 b], o3Arr ++ [O3 fx])
      O3 o3 -> (o1Arr, o3Arr ++ [O3 o3])

main :: IO ()
main = do
  let sellFuncs = map (\x -> O3 $ sellFunc x (x+5)) [1..20] 
  
  print $ func 8 sellFuncs ([],[])type Order = Bool
data OrderOutput = O1 Order | O2 (Order, Int -> OrderOutput) | O3 (Int -> OrderOutput)

instance Show OrderOutput where
  show (O1 o) = "O1 " ++ show o
  show (O2 (o, _)) = "O2 (" ++ show o ++ ", <function>)"
  show (O3 _) = "O3 <function>"

trailingTakeProfitStopLossState :: Double -> Double -> Double -> OrderOutput
trailingTakeProfitStopLossState sl tk p -- stop-loss, take-profit, price
  | p >= tk = O2 (True, trailingTakeProfitStopLossState (tk+10) (sl+10))
  | p <= sl = O1 (False)
  | otherwise = O3 (trailingTakeProfitStopLossState tk sl)

marketBuyOrder
func :: Int -> [OrderOutput] -> ([OrderOutput], [OrderOutput]) -> ([OrderOutput], [OrderOutput]) 
func _ [] oldTup = oldTup
func i (O3 f:fs) (o1Arr, o3Arr) = func i fs newTup
  where
    newTup = case f i of
      O1 o1 -> (o1Arr ++ [O1 o1], o3Arr)
      O2 (b, fx) -> (o1Arr ++ [O1 b], o3Arr ++ [O3 fx])
      O3 o3 -> (o1Arr, o3Arr ++ [O3 o3])

main :: IO ()
main = do
  let sellFuncs = map (\x -> O3 $ sellFunc x (x+5)) [1..20] 
  
  print $ func 8 sellFuncs ([],[])