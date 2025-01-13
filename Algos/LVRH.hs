module LVRH
  ( lvrhState,
    lvrhBuyLogic,
    lvrhStateTransition,
  )
where

import DataTypes (StreamData (..))

lvrhState ::
  Int -> Int -> Int -> Int -> StreamData -> (StreamData -> Int -> Int -> Int -> Int -> y) -> y
lvrhState a b c d streamData func = func streamData a b c d

lvrhStateTransition ::
  StreamData -> Int -> Int -> Int -> Int -> (StreamData -> (StreamData -> Int -> Int -> Int -> Int -> y) -> y)
lvrhStateTransition (TradeData trade) a b c d =
  let new_a = a + 10
      new_b = b + 100
   in lvrhState new_a new_b c d
lvrhStateTransition (BarData bar) a b c d =
  let new_c = c + 200
      new_d = d + 300
   in lvrhState a b new_c new_d

lvrhBuyLogic ::
  StreamData -> Int -> Int -> Int -> Int -> Bool
lvrhBuyLogic (TradeData trade) a b c d = True
lvrhBuyLogic (BarData bar) a b c d = False