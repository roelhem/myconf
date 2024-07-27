module Main where

import System.Exit (exitFailure)
import Test.QuickCheck

prop_Stub :: [Int] -> Property
prop_Stub xs = length xs === length (reverse xs)

main :: IO ()
main = quickCheck prop_Stub
