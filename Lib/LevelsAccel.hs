{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.LevelsAccel (resistanceLevelsAcc) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as GPU -- or .LLVM.Native for CPU backend


--------------------------------------------------------------------------------
-- Functions to Compute Resistance Levels
--------------------------------------------------------------------------------


resistanceLevelsAcc :: Acc (Vector Double) -> Acc (Vector Double) -> Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp Double -> Acc (Vector Double)
resistanceLevelsAcc barTopsVec barHighVec pillarThresh lowerBound upperBound rtol atol =
  A.filter (A.> 0.0) qualifyingLevelsMaskVec
  where
    areValsClose = isClose rtol atol 
    proximityMaskMat = generateProximityMaskMatrixAcc barHighVec barHighVec lowerBound upperBound areValsClose
    inBoundsMaskMat = generateInBoundsMaskMatrixAcc barTopsVec barHighVec areValsClose
    pillarMaskMat = andMatAcc proximityMaskMat inBoundsMaskMat
    pillarSumVec = A.fold (+) 0.0 pillarMaskMat
    qualifyingLevelsMaskVec = A.zipWith (\barHigh pillarSum -> cond (pillarSumn A.>= pillarThresh) barHighVec 0.0) barHighVec pillarSumVec


generateProximityMaskMatrixAcc :: Acc (Vector Double) -> Acc (Vector Double) -> Exp Double -> Exp Double -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
generateProximityMaskMatrixAcc vecA vecB lowerBound upperBound areValsClose =
  andMatAcc lowerProximityMaskMatrix upperProximityMaskMatrix
  where 
    distanceMatrix = generateDiffMatrixAcc vecA vecB
    areDistsGTELowerBoundOr = isMatGTEAcc distanceMatrix lowerBound
    areDistsLTEUpperBoundOr = isMatLTEAcc distanceMatrix upperBound
    lowerProximityMaskMatrix = areDistsGTELowerBoundOr areValsClose
    upperProximityMaskMatrix = areDistsLTEUpperBoundOr areValsClose 


generateInBoundsMaskMatrixAcc :: Acc (Vector Double) -> Acc (Vector Double) -> (Exp Double -> Exp Double -> Exp Double) -> Acc (Matrix Double)
generateInBoundsMaskMatrixAcc vecA vecB areValsClose = 
  let
    boundaryMaskMat = isMatGTAcc (generateDiffMatrixAcc vecA vecB) (0.0 :: Exp Double) areValsClose
    leftBoundaryPointsVec = generateLeftBoundaryPointsVectorAcc boundaryMaskMat
    rightBoundaryPointsVec = generateRightBoundaryPointsVectorAcc boundaryMaskMat
  in
    generate (shape boundaryMaskMat) $ \ix ->
      let Z :. row :. col = unlift ix :: Z :. Exp Int :. Exp Int 
      in cond ((col A.> (leftBoundaryPointsVec ! (index1 row))) A.&& (col A.< (rightBoundaryPointsVec ! (index1 row)))) 1.0 0.0

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
  let 
    Z :. lenA = unlift (shape vecA) :: Z :. Exp Int
    Z :. lenB = unlift (shape vecB) :: Z :. Exp Int 
    matA = A.replicate (lift (Z :. All :. lenB)) vecA
    matB = A.replicate (lift (Z :. lenA :. All)) vecB
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