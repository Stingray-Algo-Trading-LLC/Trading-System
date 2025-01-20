module BRH
  ( brhState,
    brhBuyLogic,
    brhStateTransition,
    BRHStateParam (..),
  )
where

import Data.ByteString (last)
import DataTypes (StreamData (..))

data BRHStateParam
  = BRHStateParam
  { openPrice :: Double, -- First qualifying trade price.
    highPrice :: Double, -- Highest price of all qualifying trades thus far.
    lowPrice :: Double, -- Lowest price of all qualifying trades thus far.
    lastPrice :: Double, -- Last qualifying trade price. This algo only assumes trades with condition code 1.
    openTime :: Double, -- The Unix timestamp of the last trade to update openPrice.
    firstTime :: Double, -- The Unix timestamp of the earliest trade received, condition code is irrelevant.
    lastTime :: Double -- The Unix timestamp of the last trade to update lastPrice.
  }

brhState :: BRHStateParam -> StreamData -> (BRHStateParam -> StreamData -> y) -> y
brhState stateParam streamData func = func stateParam streamData

brhStateTransition :: BRHStateParam -> StreamData -> (StreamData -> (BRHStateParam -> StreamData -> y) -> y)
brhStateTransition stateParam (TradeData trade) = brhState $ stateParam {openPrice = 10.0}
brhStateTransition stateParam (BarData bar) = brhState $ stateParam {lastPrice = 1.0}

brhBuyLogic :: BRHStateParam -> StreamData -> Bool
brhBuyLogic stateParam (TradeData trade) = False
brhBuyLogic stateParam (BarData bar) = True