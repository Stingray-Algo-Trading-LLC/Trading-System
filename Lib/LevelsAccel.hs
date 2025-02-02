{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.LevelsAccel (main) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as GPU -- or .LLVM.Native for CPU backend


resistanceLevelsX :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Scalar Double) -> Acc (Scalar Double) -> Acc (Scalar Double) -> Acc (Scalar Double) -> Acc (Scalar Double) -> Acc (Vector Double)
resistanceLevelsX barTopsVec barHighVec pillarThresh lowerBound upperBound rtol atol =


main :: IO ()
main = do 
    print "Hello from LevelsAccel.hs"

    --------------------------------------------------------------------------------
-- Functions to Compute Resistance Levels
--------------------------------------------------------------------------------
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

generateProximityMaskMatrix :: Vector Double -> Vector Double -> Double -> Double -> (Double -> Double -> Double) -> Matrix Double
generateProximityMaskMatrix vecA vecB lowerBound upperBound areValsClose =
  andMat lowerProximityMaskMatrix upperProximityMaskMatrix
  where
    distanceMatrix = generateDiffMatrix vecA vecB
    areDistsGTELowerBoundOr = isMatGTE distanceMatrix (Scalar lowerBound)
    areDistsLTEUpperBoundOr = isMatLTE distanceMatrix (Scalar upperBound)
    lowerProximityMaskMatrix = areDistsGTELowerBoundOr areValsClose
    upperProximityMaskMatrix = areDistsLTEUpperBoundOr areValsClose

generateInBoundsMaskMatrix :: Vector Double -> Vector Double -> (Double -> Double -> Double) -> Matrix Double
generateInBoundsMaskMatrix vecA vecB areValsClose =
  andMat leftBoundaryMaskMat rightBoundaryMaskMat
  where
    boundaryMaskMat = isMatGT (generateDiffMatrix vecA vecB) (Scalar 0.0) areValsClose
    cols = fromList [0.0 .. fromIntegral (size vecB - 1)]
    getDiffFromColsMat = generateDiffMatrix cols
    leftBoundaryMaskMat = isMatGT (getDiffFromColsMat $ generateLeftBoundaryPointsVector boundaryMaskMat) (Scalar 0.0) areValsClose
    rightBoundaryMaskMat = isMatLT (getDiffFromColsMat $ generateRightBoundaryPointsVector boundaryMaskMat) (Scalar 0.0) areValsClose


generateColIndxMaskMatAcc :: Acc (Matrix Double) -> Exp Double -> Acc (Matrix Double)
generateColIndxMaskMatAcc maskMat defaultVal = 
  generate (shape maskMat) $ \ix ->
    let Z :. row :. col = unlift ix :: Z :. Exp Int :. Exp Int
    in cond ((maskMat ! ix) A.== 1.0) (A.fromIntegral col) (defaultVal)

generateLeftBoundaryPointsVectorAcc :: Acc (Matrix Double) -> Acc (Vector Double) 
generateLeftBoundaryPointsVectorAcc maskMat = fold1 A.max colIndxMaskMat
  where
    colIndxMaskMat = generateColIndxMaskMatAcc maskMat (-1.0 :: Exp Double)


generateRightBoundaryPointsVectorAcc :: Acc (Matrix Double) -> Acc (Vector Double)
generateRightBoundaryPointsVectorAcc maskMat = fold1 A.min colIndxMaskMat
  where
    Z :. rows :. cols = unlift (shape maskMat) :: Z :. Exp Int :. Exp Int
    colIndxMaskMat = generateColIndxMaskMatAcc maskMat $ A.fromIntegral cols

generateDiffMatrixAcc :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Matrix Double)
generateDiffMatrixAcc vecA vecB = 
  let rows = A.shape vecA 
      cols = A.shape vecB
      matA = A.replicate (A.lift (Z :. All :. cols)) vecA 
      matB = A.replicate (A.lift (Z :. rows :. All)) vecB
  in A.zipWith (-) matA matB

-- Upper triangular mask
generateUpperTriMaskMatrixAcc :: Acc (Matrix Double) -> Acc (Matrix Double)
generateUpperTriMaskMatrixAcc matA =
    A.generate (A.shape matA) $ \ix ->
        let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
        in A.cond (i A.< j) (matA A.! ix) 0.0

  -- Lower triangular mask
generateLowerTriMaskMatrixAcc :: Acc (Matrix Double) -> Acc (Matrix Double)
generateLowerTriMaskMatrixAcc matA =
    A.generate (A.shape matA) $ \ix ->
        let Z :. i :. j = A.unlift ix :: Z :. Exp Int :. Exp Int
        in A.cond (i A.> j) (matA A.! ix) 0.0


generateLeftMaskMatrixAcc :: Acc (Matrix Double) -> Acc (Matrix Double)
generateLeftMaskMatrixAcc maskMat = generateLowerTriMaskMatrixAcc maskMat

generateRightMaskMatrixAcc :: Acc (Matrix Double) -> Acc (Matrix Double)
generateRightMaskMatrixAcc maskMat = generateUpperTriMaskMatrixAcc maskMat

isCloseAcc :: Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp Double
isClose rtol atol valA valB = A.cond (v A.<= 1) 1.0 0.0
  where
    v = A.abs (valA A.- valB) A./ (atol A.+ rtol A.* valB)

isMatEQAcc :: Acc (Matrix Double) -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
isMatEQAcc matA valB areValsClose = 
    A.map (\mElem -> A.cond (mElem A.== valB) 1.0 (areValsClose mElem valB)) matA

isMatGTAcc :: Acc (Matrix Double) -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
isMatGTAcc matA valB areValsClose = 
    A.map (\mElem -> A.cond ((mElem A.> valB) A.&& (areValsClose mElem valB A.== 0.0)) 1.0 0.0) matA

isMatLTAcc :: Acc (Matrix Double) -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
isMatLTAcc matA valB areValsClose =
    A.map (\mElem -> A.cond ((mElem A.< valB) A.&& (areValsClose mElem valB A.== 0.0)) 1.0 0.0) matA

isMatGTEAcc :: Acc (Matrix Double) -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
isMatGTEAcc matA valB areValsClose =
    A.map (\mElem -> A.cond (mElem A.>= valB) 1.0 (areValsClose mElem valB)) matA


isMatLTEAcc :: Acc (Matrix Double) -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
isMatLTEAcc matA valB areValsClose =
    A.map (\mElem -> A.cond (mElem A.<= valB) 1.0 (areValsClose mElem valB)) matA


andMatAcc :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
andMatAcc maskMatA maskMatB = A.zipWith (*) maskMatA maskMatB