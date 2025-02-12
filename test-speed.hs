{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Exception      (evaluate)
import Control.DeepSeq        (NFData, force)
import Data.Time.Clock        (getCurrentTime, diffUTCTime)
import Text.Printf            (printf)

-- hmatrix vectors
import Numeric.LinearAlgebra  as H

-- Accelerate core
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX

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

----------------------------------
-- Main
----------------------------------
main :: IO ()
main = do

  -----------------------------
  -- 1. Create or load inputs
  -----------------------------
  -- For demonstration, let's pretend we have 5 elements:
  let barTopsList  = [1.0 .. 1000.00]
      barHighsList = [101.0 .. 1100.00]
      pillarThresh = 0.5
      lowerBound   = 1.0
      upperBound   = 100.0
      rtol         = 1e-3
      atol         = 1e-6

  -- hmatrix Vectors:
  let barTopsVec  = H.fromList barTopsList
      barHighsVec = H.fromList barHighsList

  -- Accelerate Arrays:
  -- Note: fromList (Z :. length) is for Accelerate. 
  -- Then we 'use' them inside runN.
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
  let runResistanceLevels = runN $ \barTops barHighs -> resistanceLevelsAcc barTops barHighs (constant pillarThresh) (constant lowerBound) (constant upperBound) (constant rtol) (constant atol)
  (gpuResult, gpuTime) <- timeIt $ evaluate $ runResistanceLevels barTopsAcc barHighsAcc
      
   
  -----------------------------
  -- 4. Print or inspect times
  -----------------------------
  printf "CPU   version time = %f seconds\n" cpuTime
  printf "Accel version time = %f seconds\n" gpuTime

  -- If you want to see that the results are the same (within some tolerance):
  putStrLn $ "CPU   result: " Prelude.++ show cpuResult
  putStrLn $ "Accel result: " Prelude.++ show (A.toList gpuResult)
