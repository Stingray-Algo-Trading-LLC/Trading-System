{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Exception      (evaluate)
import Control.DeepSeq        (NFData, force)
import Data.Time.Clock        (getCurrentTime, diffUTCTime)
import Text.Printf            (printf)

-- hmatrix vectors
import Numeric.LinearAlgebra  as H

-- Accelerate core
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as GPU

import Lib.Levels (resistanceLevels)
import Lib.LevelsAccel (resistanceLevelsAcc)


----------------------------------
-- Timing helper
----------------------------------

-- | Time an IO action, forcing its result so we don't measure lazy evaluation later.
timeIt :: NFData a => IO a -> IO (a, Double)
timeIt ioAction = do
  start   <- getCurrentTime
  !result <- ioAction      -- bang-pattern forces the IO action itself
  end     <- getCurrentTime
  -- Use 'diffUTCTime' to get a fractional second difference
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  return (result, elapsed)


-----------------------------
-- 1. Create or load inputs
-----------------------------
barTopsList :: [Double]
barTopsList  = [1.0 .. 5000.00]

barHighsList :: [Double]
barHighsList = [101.0 .. 5100.00]

pillarThresh :: Double
pillarThresh = 0.5

lowerBound :: Double
lowerBound   = 1.0

upperBound :: Double
upperBound   = 100.0

rtol :: Double
rtol         = 1e-3

atol :: Double
atol         = 1e-6


----------------------------------
-- Main
----------------------------------
main :: IO ()
main = do

  

  -- hmatrix Vectors:
  let barTopsVec  = H.fromList barTopsList
      barHighsVec = H.fromList barHighsList

  -- Accelerate Arrays:
  -- Note: fromList (Z :. length) is for Accelerate. 
  let barTopsAcc  = A.fromList (Z :. Prelude.length barTopsList)  barTopsList
      barHighsAcc = A.fromList (Z :. Prelude.length barHighsList) barHighsList

  ---------------------------------------
  -- 2. Time the “CPU/hmatrix” function
  ---------------------------------------
  (cpuResult, cpuTime) <- timeIt $ evaluate $
    resistanceLevels
      barTopsVec
      barHighsVec
      pillarThresh
      lowerBound
      upperBound
      rtol
      atol

  ---------------------------------------
  -- 3. Time the Accelerate version
  ---------------------------------------
  -- Here we compile a “runN” version that can be reused.
  let runResistanceLevels = $(GPU.runQ $ \barTops barHighs -> resistanceLevelsAcc barTops barHighs (constant 0.5) (constant 1.0) (constant 100.0) (constant 1e-3) (constant 1e-6))
  (gpuResult, gpuTime) <- timeIt $ evaluate $ runResistanceLevels barTopsAcc barHighsAcc
      
   
  -----------------------------
  -- 4. Print or inspect times
  -----------------------------
  printf "CPU   version time = %f seconds\n" cpuTime
  printf "Accel version time = %f seconds\n" gpuTime

  -- If you want to see that the results are the same (within some tolerance):
  -- putStrLn $ "CPU   result: " Prelude.++ show cpuResult
  -- putStrLn $ "Accel result: " Prelude.++ show (A.toList gpuResult)
  putStrLn $ "Equal? " Prelude.++ show (H.toList cpuResult Prelude.== A.toList gpuResult)