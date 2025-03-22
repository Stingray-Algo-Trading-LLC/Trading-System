module Lib.Hammer (isGreenHammer, isHammerInResDeflectZone) where

isGreenHammer :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
isGreenHammer minBodyLen maxUpperWickLen minLowerWickLen open high low last =
    last - open >= minBodyLen && 
    high - last <= maxUpperWickLen &&
    open - low >= minLowerWickLen


isHammerInResDeflectZone :: Double -> Double -> Double -> [Double] -> (Bool, Double)
isHammerInResDeflectZone tolerance low last sortedResLevels = 
    case (length $ takeWhile (<=last) (sortedResLevels)) - 1 of
        -1 -> (False, -1.0)
        i -> (abs (low - sortedResLevels !! i) <= tolerance, sortedResLevels !! i)