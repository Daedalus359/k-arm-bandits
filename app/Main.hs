module Main where

import KAB

main :: IO ()
main = putStrLn testComp

--withSystemRandom (genContVar g :: (Gen (PrimState IO) -> IO Double))