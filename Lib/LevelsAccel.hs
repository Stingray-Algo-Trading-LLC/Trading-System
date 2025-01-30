
module Lib.LevelsAccel (main) where
    
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX as GPU -- or .LLVM.Native for CPU backend


main :: IO ()
main = do 
    print "Hello from LevelsAccel.hs"