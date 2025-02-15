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
barTopsList400 :: [Double]
barTopsList400  = [1.0 .. 400]

barHighsList400 :: [Double]
barHighsList400 = [101.0 .. 500]

barTopsList6000 :: [Double]
barTopsList6000  = [1.0 .. 6000]

barHighsList6000 :: [Double]
barHighsList6000 = [101.0 .. 6100]

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
  let barTopsVec400  = H.fromList barTopsList400
      barHighsVec400 = H.fromList barHighsList400

  -- Accelerate Arrays:
  -- Note: fromList (Z :. length) is for Accelerate. 
  let barTopsAcc400  = A.fromList (Z :. Prelude.length barTopsList400)  barTopsList400 :: A.Vector Double
      barHighsAcc400 = A.fromList (Z :. Prelude.length barHighsList400) barHighsList400 :: A.Vector Double

  
  -- hmatrix Vectors:
  let barTopsVec6000  = H.fromList barTopsList6000
      barHighsVec6000 = H.fromList barHighsList6000

  -- Accelerate Arrays:
  -- Note: fromList (Z :. length) is for Accelerate. 
  let barTopsAcc6000  = A.fromList (Z :. Prelude.length barTopsList6000)  barTopsList6000 :: A.Vector Double
      barHighsAcc6000 = A.fromList (Z :. Prelude.length barHighsList6000) barHighsList6000 :: A.Vector Double

  ---------------------------------------
  -- 2. Time the “CPU/hmatrix” function
  ---------------------------------------
  (cpuResult400, cpuTime400) <- timeIt $ evaluate $
    resistanceLevels
      barTopsVec400
      barHighsVec400
      pillarThresh
      lowerBound
      upperBound
      rtol
      atol

  (cpuResult6000, cpuTime6000) <- timeIt $ evaluate $
    resistanceLevels
      barTopsVec6000
      barHighsVec6000
      pillarThresh
      lowerBound
      upperBound
      rtol
      atol

  ---------------------------------------
  -- 3. Time the Accelerate version
  ---------------------------------------
  -- Here we compile a “runN” version that can be reused.
  let resistanceLevelsGPU :: A.Vector Double -> A.Vector Double -> A.Vector Double
      resistanceLevelsGPU = $(GPU.runQ $ \barTops barHighs -> 
        resistanceLevelsAcc barTops barHighs (constant 0.5) (constant 1.0) (constant 100.0) (constant 1e-3) (constant 1e-6))
  
  (gpuResult400, gpuTime400) <- timeIt $ evaluate $ resistanceLevelsGPU barTopsAcc400 barHighsAcc400
  
  (gpuResult6000, gpuTime6000) <- timeIt $ evaluate $ resistanceLevelsGPU barTopsAcc6000 barHighsAcc6000
   
  -----------------------------
  -- 4. Print or inspect times
  -----------------------------
  printf "CPU400   version time = %f seconds\n" cpuTime400
  printf "GPU400   version time = %f seconds\n" gpuTime400

  printf "\nCPU6000 version time = %f seconds\n" cpuTime6000
  printf "GPU6000   version time = %f seconds\n" gpuTime6000


  -- If you want to see that the results are the same (within some tolerance):
  -- putStrLn $ "CPU   result: " Prelude.++ show cpuResult
  -- putStrLn $ "Accel result: " Prelude.++ show (A.toList gpuResult)
  putStrLn $ "CPU400 Equal GPU400? " Prelude.++ show (H.toList cpuResult400 Prelude.== A.toList gpuResult400)
  putStrLn $ "CPU6000 Equal GPU6000? " Prelude.++ show (H.toList cpuResult6000 Prelude.== A.toList gpuResult6000)
