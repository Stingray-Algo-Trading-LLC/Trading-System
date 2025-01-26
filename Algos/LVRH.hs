module Algos.LVRH
  ( lvrhState,
    lvrhBuyLogic,
    lvrhStateTransition,
    LVRHStateParam (..),
  )
where

import Data.Ord (Ord (max, min))
import Lib.DataTypes (Bar (..), StreamData (..), Trade (..))
import Lib.Utils (getSaleCondition, utcToUnixSeconds)
import Numeric.LinearAlgebra

data LVRHStateParam
  = LVRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    openTime :: Double, -- The Unix timestamp of the last trade to update openPrice.
    firstTime :: Double, -- The Unix timestamp of the earliest trade received, condition code is irrelevant.
    lastTime :: Double, -- The Unix timestamp of the last trade to update lastPrice.
    barTops :: Vector Double, -- Sequence of bar top prices. Open/Close price for Red/Green bars.
    barHighs :: Vector Double, -- Sequence of bar high prices.
    resLines :: Vector Double -- Resistance price levels.
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
      { openPrice = 1 / 0, -- infinity
        highPrice = -(1 / 0), -- -infinity
        lowPrice = 1 / 0, -- infinity
        lastPrice = 0.0,
        openTime = 1 / 0, -- infinity
        firstTime = 1 / 0, -- infinity
        lastTime = -(1 / 0), -- -infinity
        barTops = newBarTops,
        barHighs = newBarHighs,
        resLines = newBarHighs -- getResLines newBarTops newBarHighs
      }
  where
    newBarTops = vjoin [barTops stateParams, fromList [max (open bar) (close bar)]]
    newBarHighs = vjoin [barHighs stateParams, fromList [high bar]]

lvrhBuyLogic :: LVRHStateParam -> StreamData -> Bool
lvrhBuyLogic stateParams (TradeData trade) = True
lvrhBuyLogic stateParams (BarData bar) = False