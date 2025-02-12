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
  A.afst (A.filter (A.> 0.0) qualifyingLevelsMaskVec)
  where
    areValsClose = isCloseAcc rtol atol 
    proximityMaskMat = generateProximityMaskMatrixAcc barHighVec barHighVec lowerBound upperBound areValsClose
    inBoundsMaskMat = generateInBoundsMaskMatrixAcc barTopsVec barHighVec areValsClose
    pillarMaskMat = andMatAcc proximityMaskMat inBoundsMaskMat
    pillarSumVec = A.fold (A.+) 0.0 pillarMaskMat
    qualifyingLevelsMaskVec = A.zipWith (\barHigh pillarSum -> A.cond (pillarSum A.>= pillarThresh) barHigh 0.0) barHighVec pillarSumVec


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
      let Z :. row :. col = A.unlift ix :: Z :. Exp Int :. Exp Int 
      in A.cond ((A.fromIntegral col A.> (leftBoundaryPointsVec A.! A.index1 row)) A.&& (A.fromIntegral col A.< (rightBoundaryPointsVec A.! A.index1 row))) 1.0 0.0

generateColIndxMaskMatAcc :: Acc (Matrix Double) -> Exp Double -> Acc (Matrix Double)
generateColIndxMaskMatAcc maskMat defaultVal = 
  generate (shape maskMat) $ \ix ->
    let Z :. row :. col = A.unlift ix :: Z :. Exp Int :. Exp Int
    in A.cond ((maskMat A.! ix) A.== 1.0) (A.fromIntegral col) defaultVal

generateLeftBoundaryPointsVectorAcc :: Acc (Matrix Double) -> Acc (Vector Double) 
generateLeftBoundaryPointsVectorAcc maskMat = A.fold1 A.max colIndxMaskMat
  where
    leftMaskMat = generateLowerTriMaskMatrixAcc maskMat
    colIndxMaskMat = generateColIndxMaskMatAcc leftMaskMat (-1.0 :: Exp Double)


generateRightBoundaryPointsVectorAcc :: Acc (Matrix Double) -> Acc (Vector Double)
generateRightBoundaryPointsVectorAcc maskMat = A.fold1 A.min colIndxMaskMat
  where
    Z :. rows :. cols = A.unlift (shape maskMat) :: Z :. Exp Int :. Exp Int
    rightMaskMat = generateUpperTriMaskMatrixAcc maskMat
    colIndxMaskMat = generateColIndxMaskMatAcc rightMaskMat $ A.fromIntegral cols

generateDiffMatrixAcc :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Matrix Double)
generateDiffMatrixAcc vecA vecB = 
  let 
    Z :. lenA = A.unlift (A.shape vecA) :: Z :. Exp Int
    Z :. lenB = A.unlift (A.shape vecB) :: Z :. Exp Int 
    matA = A.replicate (A.lift (Z :. lenB :. All)) vecA
    matB = A.replicate (A.lift (Z :. All :. lenA)) vecB
  in A.zipWith (A.-) matA matB


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


isCloseAcc :: Exp Double -> Exp Double -> Exp Double -> Exp Double -> Exp Double
isCloseAcc rtol atol valA valB = A.cond (v A.<= 1) 1.0 0.0
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
andMatAcc = A.zipWith (A.*)