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
barTopsList400  = [500.00, 502.00 .. 1300.0]

barHighsList400 :: [Double]
barHighsList400 = [11.0, 14.0 .. 1211.0]

barTopsList6000 :: [Double]
barTopsList6000  = [2.0, 4.0 .. 12002.0]

barHighsList6000 :: [Double]
barHighsList6000 = [10.0, 20.0 .. 60010.0]

barTopsList100 :: [Double]
barTopsList100  = [5.0, 10.0 .. 505.0]

barHighsList100 :: [Double]
barHighsList100 = [1.0 .. 101.0]

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

  -- hmatrix Vectors:
  let barTopsVec100  = H.fromList barTopsList100
      barHighsVec100 = H.fromList barHighsList100

  -- Accelerate Arrays:
  -- Note: fromList (Z :. length) is for Accelerate. 
  let barTopsAcc100  = A.fromList (Z :. Prelude.length barTopsList100)  barTopsList100 :: A.Vector Double
      barHighsAcc100 = A.fromList (Z :. Prelude.length barHighsList100) barHighsList100 :: A.Vector Double
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

  (cpuResult100, cpuTime100) <- timeIt $ evaluate $
    resistanceLevels
      barTopsVec100
      barHighsVec100
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

  (gpuResult100, gpuTime100) <- timeIt $ evaluate $ resistanceLevelsGPU barTopsAcc100 barHighsAcc100
  
  
  
   
  -----------------------------
  -- 4. Print or inspect times
  -----------------------------
  printf "CPU400   version time = %f seconds\n" cpuTime400
  printf "GPU400   version time = %f seconds\n" gpuTime400

  printf "\nCPU6000 version time = %f seconds\n" cpuTime6000
  printf "GPU6000   version time = %f seconds\n" gpuTime6000

  printf "\nCPU100 version time = %f seconds\n" cpuTime100
  printf "GPU100   version time = %f seconds\n" gpuTime100

  -- If you want to see that the results are the same (within some tolerance):
  -- putStrLn $ "CPU   result: " Prelude.++ show cpuResult
  -- putStrLn $ "Accel result: " Prelude.++ show (A.toList gpuResult)
  putStrLn $ "\nCPU400 Equal GPU400? " Prelude.++ show (H.toList cpuResult400 Prelude.== A.toList gpuResult400)
  putStrLn $ "CPU6000 Equal GPU6000? " Prelude.++ show (H.toList cpuResult6000 Prelude.== A.toList gpuResult6000)
  putStrLn $ "CPU100 Equal GPU100? " Prelude.++ show (H.toList cpuResult100 Prelude.== A.toList gpuResult100)