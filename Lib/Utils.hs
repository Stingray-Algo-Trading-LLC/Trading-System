{-# LANGUAGE ImportQualifiedPost #-}

module Utils (getSaleCondition, utcToUnixSeconds) where

import Data.Char (ord)
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Real (fromIntegral)
import Numeric.LinearAlgebra

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

--------------------------------------------------------------------------------
-- Convert UTCTimestamp to Unix Seconds
--------------------------------------------------------------------------------
utcToUnixSeconds :: UTCTime -> Double
utcToUnixSeconds utcTime = realToFrac (utcTimeToPOSIXSeconds utcTime)

--------------------------------------------------------------------------------
-- Functions to Compute Resistance Levels
--------------------------------------------------------------------------------
diffMatrix :: Vector Double -> Vector Double -> Matrix Double
diffMatrix vecA vecB = asRow vecA - asColumn vecB

-- Upper triangular mask
generateUpperTriMaskMatrix :: (Int, Int) -> Int -> Matrix Double
generateUpperTriMaskMatrix (rows, cols) k =
  fromLists [[if j >= i + k then 1.0 else 0.0 | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]

-- Lower triangular mask
generateLowerTriMaskMatrix :: (Int, Int) -> Int -> Matrix Double
generateLowerTriMaskMatrix (rows, cols) k =
  fromLists [[if j <= i + k then 1.0 else 0.0 | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]

generateLeftMaskMatrix :: Matrix Double -> Matrix Double
generateLeftMaskMatrix maskMat = andMat maskMat (generateLowerTriMaskMatrix (size maskMat) (-1))

generateRightMaskMatrix :: Matrix Double -> Matrix Double
generateRightMaskMatrix maskMat = andMat maskMat (generateUpperTriMaskMatrix (size maskMat) 1)

generateLeftBoundaryPointsVector :: Matrix Double -> Vector Double
generateLeftBoundaryPointsVector maskMat =
  fromList [findLeftPoint revRow | revRow <- toRows $ fliprl maskMat]
  where
    findLeftPoint revRow =
      let maxIndxInRevRow = maxIndex revRow
          valAtMaxIndxOfRevRow = floor (atIndex revRow maxIndxInRevRow) -- Either 1 or 0.
          -- Left boundary point is -1.0 if no boundary points (ie. no 1.0's). Otherwise,
          -- left boundary point is the index of the last 1.0, hence the formula:
          -- size revRow - maxIndxInRevRow - 1.
       in [-1.0, fromIntegral (size revRow - maxIndxInRevRow - 1)] !! valAtMaxIndxOfRevRow

generateRightBoundaryPointsVector :: Matrix Double -> Vector Double
generateRightBoundaryPointsVector maskMat =
  fromList [findRightPoint row | row <- toRows maskMat]
  where
    findRightPoint row =
      let maxIndxInRow = maxIndex row
          valAtMaxIndxOfRow = floor (atIndex row maxIndxInRow) -- Either 1 or 0.
          -- Right boundary point is row size if no boundary points (ie. no 1.0's).
          -- Otherwise, it is index of the first 1.0.
       in [fromIntegral $ size row, fromIntegral maxIndxInRow] !! valAtMaxIndxOfRow

generateProximityMaskMatrix :: Vector Double -> Vector Double -> Double -> Double -> (Double -> Double -> Double) -> Matrix Double
generateProximityMaskMatrix vecA vecB lowerBound upperBound areValsClose =
  andMat lowerProximityMaskMatrix upperProximityMaskMatrix
  where
    distanceMatrix = diffMatrix vecA vecB
    areDistsGTELowerBoundOr = isMatGTE distanceMatrix (Scalar lowerBound)
    areDistsLTEUpperBoundOr = isMatLTE distanceMatrix (Scalar upperBound)
    lowerProximityMaskMatrix = areDistsGTELowerBoundOr areValsClose
    upperProximityMaskMatrix = areDistsLTEUpperBoundOr areValsClose

isClose :: Double -> Double -> Double -> Double -> Double
isClose rtol atol valA valB = if v <= 1 then 1.0 else 0.0
  where
    v = abs (valA - valB) / (atol + rtol * valB)

data MatrixOrScalar = Scalar Double | MatD (Matrix Double)

isMatEQ :: Matrix Double -> MatrixOrScalar -> (Double -> Double -> Double) -> Matrix Double
isMatEQ matA (Scalar valB) areValsClose = cmap (\mElem -> if mElem == valB then 1.0 else areValsClose mElem valB) matA
isMatEQ matA (MatD matB) areValsClose = cmap (\mElem -> if mElem == 0.0 then 1.0 else areValsClose mElem 0.0) (matA - matB)

isMatGT :: Matrix Double -> MatrixOrScalar -> (Double -> Double -> Double) -> Matrix Double
isMatGT matA (Scalar valB) areValsClose =
  cmap (\mElem -> if (mElem > valB) && (areValsClose mElem valB == 0.0) then 1.0 else 0.0) matA
isMatGT matA (MatD matB) areValsClose =
  cmap (\mElem -> if (mElem > 0.0) && (areValsClose mElem 0.0 == 0.0) then 1.0 else 0.0) (matA - matB)

isMatLT :: Matrix Double -> MatrixOrScalar -> (Double -> Double -> Double) -> Matrix Double
isMatLT matA (Scalar valB) areValsClose =
  cmap (\mElem -> if (mElem < valB) && (areValsClose mElem valB == 0.0) then 1.0 else 0.0) matA
isMatLT matA (MatD matB) areValsClose =
  cmap (\mElem -> if (mElem < 0.0) && (areValsClose mElem 0.0 == 0.0) then 1.0 else 0.0) (matA - matB)

isMatGTE :: Matrix Double -> MatrixOrScalar -> (Double -> Double -> Double) -> Matrix Double
isMatGTE matA (Scalar valB) areValsClose =
  cmap (\mElem -> if mElem >= valB then 1.0 else areValsClose mElem valB) matA
isMatGTE matA (MatD matB) areValsClose =
  cmap (\mElem -> if mElem >= 0.0 then 1.0 else areValsClose mElem 0.0) (matA - matB)

isMatLTE :: Matrix Double -> MatrixOrScalar -> (Double -> Double -> Double) -> Matrix Double
isMatLTE matA (Scalar valB) areValsClose =
  cmap (\mElem -> if mElem <= valB then 1.0 else areValsClose mElem valB) matA
isMatLTE matA (MatD matB) areValsClose =
  cmap (\mElem -> if mElem <= 0.0 then 1.0 else areValsClose mElem 0.0) (matA - matB)

andMat :: Matrix Double -> Matrix Double -> Matrix Double
andMat maskMatA maskMatB = maskMatA * maskMatB

generateInBoundsMaskMatrix :: Vector Double -> Vector Double -> (Double -> Double -> Double) -> Matrix Double
generateInBoundsMaskMatrix vecA vecB areValsClose =
  andMat leftBoundaryMaskMat rightBoundaryMaskMat
  where
    boundaryMaskMat = isMatGT (diffMatrix vecA vecB) (Scalar 0.0) areValsClose
    cols = fromList [0.0 .. fromIntegral (size vecB - 1)]
    getDiffFromColsMat = diffMatrix cols
    leftBoundaryMaskMat = isMatGT (getDiffFromColsMat $ generateLeftBoundaryPointsVector boundaryMaskMat) (Scalar 0.0) areValsClose
    rightBoundaryMaskMat = isMatLT (getDiffFromColsMat $ generateRightBoundaryPointsVector boundaryMaskMat) (Scalar 0.0) areValsClose

resistanceLevels :: Vector Double -> Vector Double -> Double -> Double -> Double -> Double -> Double -> Vector Double
resistanceLevels barTopsVec barHighVec pillarThresh lowerBound upperBound rtol atol =
  fromList (filter (> 0.0) (toList qualifyingLevelsMaskVec))
  where
    areValsClose = isClose rtol atol
    proximityMaskMat = generateProximityMaskMatrix barHighVec barHighVec lowerBound upperBound areValsClose
    inBoundsMaskMat = generateInBoundsMaskMatrix barTopsVec barHighVec areValsClose
    pillarMaskMat = andMat proximityMaskMat inBoundsMaskMat
    qualifyingLevelsMaskVec =
      fromList (map (\row -> if sumElements row >= pillarThresh then 1.0 else 0.0) (toRows pillarMaskMat))
        * barHighVec