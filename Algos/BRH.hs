module Algos.BRH
  ( brhBuyLogic,
    brhStateTransition,
    BRHState,
    initBRH
  )
where

import Lib.DataTypes (Bar (..), StreamData (..), Trade (..))
import Lib.Hammer (isGreenHammer, isHammerInResDeflectZone)
import Lib.LevelsAccel (initResistanceLevelsAcc)
import Lib.Levels (resistanceLevels)
import Data.Ord (Ord (max, min))
import Data.List (sort)

data BRHState = BRHState (forall y. StreamData -> (BRHStateParam -> StreamData -> y) -> y)

data BRHStateParam
  = BRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    barTops :: [Double], -- Sequence of bar top prices. Open/Close price for Red/Green bars.
    barHighs :: [Double], -- Sequence of bar high prices.
    resLevels :: [Double], -- Resistance price levels.
    genResLevels :: [Double] -> [Double] -> [Double],
    isHammer :: Double -> Double -> Double -> Double -> Bool,
    inDeflectZone :: Double -> Double -> [Double] -> Bool
  }

brhState :: BRHStateParam -> StreamData -> (BRHStateParam -> StreamData -> y) -> y
brhState stateParam streamData func = func stateParam streamData

brhStateTransition :: BRHStateParam -> StreamData -> (StreamData -> (BRHStateParam -> StreamData -> y) -> y)
brhStateTransition stateParam (TradeData trade) = brhState $ stateParam
brhStateTransition stateParam (BarData bar) = 
  brhState $ stateParam 
    { openPrice = open bar,
      highPrice = high bar,
      lowPrice = low bar,
      lastPrice = close bar,
      barTops = newBarTops,
      barHighs = newBarHighs,
      resLevels = newResLevels -- getResLevels newBarTops newBarHighs
    }
  where
    newBarTops = barTops stateParams ++ [max (open bar) (close bar)]
    newBarHighs = barHighs stateParams ++ [high bar]
    newResLevels = sort $ (genResLevels stateParams) newBarHighs newBarHighs


brhBuyLogic :: Bool -> BRHStateParam -> StreamData -> Bool
brhBuyLogic buyStateParams stateParam (TradeData trade) = False
brhBuyLogic _ stateParam (BarData bar) =
  isHammer stateParams 
    (openPrice stateParams) (highPrice stateParams) (lowPrice stateParams) (lastPrice stateParams) &&
  inDeflectZone stateParams 
    (lowPrice stateParams) (lastPrice stateParams) (resLevels stateParams)  -- Wick "touches" or is near resistance level.

initBRH :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double  -> LVRHState
initBRH minResPillars resTolerance minBodyLen maxUpperWickLen minLowerWickLen deflecTolerance rtol atol =
  brhState $ BRHStateParam 
    {
      openPrice = 1/0, 
      highPrice = -(1/0),
      lowPrice = 1/0, 
      lastPrice = 0.0,
      barTops = [], 
      barHighs = [],
      resLevels = [], 
      genResLevels = initResistanceLevelsAcc minResPillars resTolerance rtol atol,
      isHammer = isGreenHammer minBodyLen maxUpperWickLen minLowerWickLen,
      inDeflectZone = isHammerInResDeflectZone deflecTolerance
    }