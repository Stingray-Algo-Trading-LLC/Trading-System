module BRH
  ( brhState,
    brhBuyLogic,
    brhStateTransition,
  )
where

import DataTypes (StreamData (..))

brhState ::
  Int -> Int -> StreamData -> (StreamData -> Int -> Int -> y) -> y
brhState a b streamData func = func streamData a b

brhStateTransition ::
  StreamData -> Int -> Int -> (StreamData -> (StreamData -> Int -> Int -> y) -> y)
brhStateTransition (TradeData trade) a b =
  let new_a = a + 1000
   in brhState new_a b
brhStateTransition (BarData bar) a b =
  let new_b = b + 2000
   in brhState a new_b

brhBuyLogic ::
  StreamData -> Int -> Int -> Bool
brhBuyLogic (TradeData trade) a b = False
brhBuyLogic (BarData bar) a b = True