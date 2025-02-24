module Algos.BRH
  ( brhState,
    brhBuyLogic,
    brhStateTransition,
    BRHStateParam (..),
  )
where

import Lib.DataTypes (StreamData (..))

data BRHStateParam
  = BRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    barTops :: [Double], -- Sequence of bar top prices. Open/Close price for Red/Green bars.
    barHighs :: [Double], -- Sequence of bar high prices.
    resLevels :: [Double], -- Resistance price levels.
    genResLevels :: [Double] -> [Double] -> [Double]
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
  lastPrice stateParams > openPrice stateParams + 0.05 &&  -- Bar body length.
  lastPrice stateParams == highPrice stateParams &&        -- Last price is highest point of bar.
  openPrice stateParams - lowPrice stateParams >= 0.15 &&  -- Bottom Wick length.
  inDeflectionZone (lowPrice stateParams) (resLevels stateParams) (lastPrice stateParams) 0.02 -- Wick "touches" or is near resistance level.

inDeflectionZone :: Double -> [Double] -> Double -> Double -> Bool
inDeflectionZone point sortedResLevels lastPrice tolerance = 
  case (length $ takeWhile (<=lastPrice) (sortedResLevels)) - 1 of
    -1 -> False
    i -> abs (point - sortedResLevels !! i) <= tolerance