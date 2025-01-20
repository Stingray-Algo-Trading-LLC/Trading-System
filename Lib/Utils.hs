{-# LANGUAGE ImportQualifiedPost #-}

module Utils (getSaleCondition) where

import Data.Char (ord)
import Data.Set qualified as Set

--------------------------------------------------------------------------------
-- Classifies a Trade's Condition List as Either 0, 1, 2, or 3
-- as per guidance given by SIP in UTP and CTS Specs.
--------------------------------------------------------------------------------
getSaleCondition :: [Char] -> Int
getSaleCondition conditions = if conditionCode == 6 then 2 else conditionCode
  where
    conditionCode =
      Set.foldr (*) 1 (Set.fromList [if ord c >= 0 && ord c <= 90 then conditionLookUpTable !! ord c else 0 | c <- conditions])
    conditionLookUpTable =
      replicate 32 0
        ++ [1]
        ++ replicate 16 0
        ++ [1, 0, 0, 2, 1, 1, 0, 0, 1]
        ++ replicate 7 0
        ++ [1, 0, 0, 1, 1, 1, 2, 0, 0, 0, 1, 3, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 1, 1, 2]
