module LVRH
  ( lvrhState,
    lvrhBuyLogic,
    lvrhStateTransition,
    LVRHStateParam (..),
  )
where

import DataTypes (StreamData (..))
import Utils (getSaleCondition)

data LVRHStateParam
  = LVRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    openTime :: Double, -- The Unix timestamp of the last trade to update openPrice.
    firstTime :: Double, -- The Unix timestamp of the earliest trade received, condition code is irrelevant.
    lastTime :: Double -- The Unix timestamp of the last trade to update lastPrice.
  }

lvrhState :: LVRHStateParam -> StreamData -> (LVRHStateParam -> StreamData -> y) -> y
lvrhState stateParams streamData func = func stateParams streamData

lvrhStateTransition :: LVRHStateParam -> StreamData -> (StreamData -> (LVRHStateParam -> StreamData -> y) -> y)
lvrhStateTransition stateParams (TradeData trade) = lvrhState $ stateParams {openPrice = 10.0}
lvrhStateTransition stateParams (BarData bar) = lvrhState $ stateParams {lastPrice = 1.0}

lvrhBuyLogic :: LVRHStateParam -> StreamData -> Bool
lvrhBuyLogic stateParams (TradeData trade) = True
lvrhBuyLogic stateParams (BarData bar) = False