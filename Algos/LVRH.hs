module Algos.LVRH
  ( lvrhBuyLogic,
    lvrhStateTransition,
    LVRHState,
    initLVRH
  )
where

import Data.Ord (Ord (max, min))
import Lib.DataTypes (Bar (..), StreamData (..), Trade (..))
import Lib.Utils (getSaleCondition, utcToUnixSeconds)
import Lib.Hammer (isGreenHammer, isHammerInResDeflectZone)
import Lib.LevelsAccel (initResistanceLevelsAcc)
import Lib.Levels (resistanceLevels)
import Data.List (sort)

data LVRHState = LVRHState (forall y. StreamData -> (LVRHStateParam -> StreamData -> y) -> y)

data LVRHStateParam
  = LVRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    openTime :: Double, -- The Unix timestamp of the last trade to update openPrice.
    firstTime :: Double, -- The Unix timestamp of the earliest trade received, condition code is irrelevant.
    lastTime :: Double, -- The Unix timestamp of the last trade to update lastPrice.
    barTops :: [Double], -- Sequence of bar top prices. Open/Close price for Red/Green bars.
    barHighs :: [Double], -- Sequence of bar high prices.
    resLevels :: [Double], -- Resistance price levels.
    genResLevels :: [Double] -> [Double] -> [Double],
    hasDelayTimeElapsed :: Double -> Double -> Bool,
    isHammer :: Double -> Double -> Double -> Double -> Bool,
    inDeflectZone :: Double -> Double -> [Double] -> Bool
  }
  deriving (Show)

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
            lastPrice = if (saleCondition == 1) && (currTradeTime >= lastTime stateParams) then price trade else lastPrice stateParams,
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
        lastPrice = 0.0,
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
    (openPrice stateParams) (highPrice stateParams) (lowPrice stateParams) (lastPrice stateParams) &&
  inDeflectZone stateParams 
    (lowPrice stateParams) (lastPrice stateParams) (resLevels stateParams)  -- Wick "touches" or is near resistance level.

initLVRH :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> LVRHState
initLVRH minResPillars resTolerance minBodyLen maxUpperWickLen minLowerWickLen deflecTolerance delay rtol atol =
  lvrhState $ LVRHStateParam 
    {
      openPrice = 1/0, 
      highPrice = -(1/0),
      lowPrice = 1/0, 
      lastPrice = 0.0,
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
