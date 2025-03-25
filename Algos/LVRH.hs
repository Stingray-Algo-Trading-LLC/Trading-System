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

data LVRHState = LVRHState (forall y. StreamData -> (LVRHStateParam -> StreamData -> y) -> y)



type Order = Bool
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

lvrhState :: LVRHStateParam -> StreamData -> (LVRHStateParam -> StreamData -> y) -> y
lvrhState stateParams streamData func = func stateParams streamData

lvrhStateTransition :: LVRHStateParam -> StreamData -> (StreamData -> (LVRHStateParam -> StreamData -> y) -> y)
lvrhStateTransition stateParams (TradeData trade) =
  if saleCondition == 0
    then
      lvrhState $
        stateParams {firstTime = min (firstTime stateParams) currTradeTime}
    else
      lvrhState $
        stateParams
          { openPrice = if currTradeTime < openTime stateParams then price trade else openPrice stateParams,
            highPrice = max (highPrice stateParams) (price trade),
            lowPrice = min (lowPrice stateParams) (price trade),
            closePrice = if (saleCondition == 1) && (currTradeTime >= lastTime stateParams) then price trade else closePrice stateParams,
            openTime = min (openTime stateParams) currTradeTime,
            firstTime = min (firstTime stateParams) currTradeTime,
            lastTime = if saleCondition == 1 then max (lastTime stateParams) currTradeTime else lastTime stateParams
          }
  where
    saleCondition = getSaleCondition $ condition trade
    currTradeTime = utcToUnixSeconds $ tradeTimestamp trade
lvrhStateTransition stateParams (BarData bar) =
  lvrhState $
    stateParams
      { openPrice = 1/0, -- infinity
        highPrice = -(1/0), -- -infinity
        lowPrice = 1/0, -- infinity
        closePrice = 0.0,
        openTime = 1/0, -- infinity
        firstTime = 1/0, -- infinity
        lastTime = -(1/0), -- -infinity
        barTops = newBarTops,
        barHighs = newBarHighs,
        resLevels = newResLevels -- getResLevels newBarTops newBarHighs
      }
  where
    newBarTops = barTops stateParams ++ [max (open bar) (close bar)]
    newBarHighs = barHighs stateParams ++ [high bar]
    newResLevels = sort $ (genResLevels stateParams) newBarHighs newBarHighs

lvrhBuyLogic :: Bool -> LVRHStateParam -> StreamData -> Bool
lvrhBuyLogic buyStateParams stateParams (BarData bar) = False
lvrhBuyLogic buyStateParams stateParams (TradeData trade) = 
  not buyStateParams &&
  hasDelayTimeElapsed stateParams 
    (firstTime stateParams) (lastTime stateParams) && -- Wait time to ensure open price is accurate.
  isHammer stateParams 
    (openPrice stateParams) (highPrice stateParams) (lowPrice stateParams) (closePrice stateParams) &&
  inDeflectZone stateParams 
    (lowPrice stateParams) (closePrice stateParams) (resLevels stateParams)  -- Wick "touches" or is near resistance level.

initLVRH :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> LVRHState
initLVRH minResPillars resTolerance minBodyLen maxUpperWickLen minLowerWickLen deflecTolerance delay rtol atol =
  lvrhState $ LVRHStateParam 
    {
      openPrice = 1/0, 
      highPrice = -(1/0),
      lowPrice = 1/0, 
      closePrice = 0.0,
      openTime = 1/0,
      firstTime = 1/0, 
      lastTime = -(1/0),
      barTops = [], 
      barHighs = [],
      resLevels = [], 
      genResLevels = initResistanceLevelsAcc minResPillars resTolerance rtol atol,
      hasDelayTimeElapsed = \firstTime lastTime -> lastTime - firstTime >= delay :: Double -> Double -> Bool,
      isHammer = isGreenHammer minBodyLen maxUpperWickLen minLowerWickLen,
      inDeflectZone = isHammerInResDeflectZone deflecTolerance
    }


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
assetStateMachine :: ConstantState -> AssetStateTransition
assetStateMachine constState = AssetStateTransition assetStateTrans 
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
        newResLevels = sort $ (genResLevels constState) newBarHighs newBarHighs
    assetStateTrans ohlcSt levelSt timeSt (BarUpdateData bar) = (ohlcSt, newLevelSt, timeSt, assetStateTrans ohlcSt newLevelSt timeSt)
      where
        newLevelSt = levelSt
          { barTops = newBarTops,
            barHighs = newBarHighs,
            resLevels = newResLevels -- getResLevels newBarTops newBarHighs
          }
        newBarTops = safeInit (barTops levelSt) ++ [max (open bar) (close bar)]
        newBarHighs = safeInit (barHighs levelSt) ++ [high bar]
        newResLevels = sort $ (genResLevels constState) newBarHighs newBarHighs

        