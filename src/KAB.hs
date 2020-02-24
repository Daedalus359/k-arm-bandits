module KAB where

import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

testComp = "."

normalDist = normalDistr 0 1

sampleNormal :: IO Double
sampleNormal = withSystemRandom (genContVar normalDist :: (GenIO -> IO Double))