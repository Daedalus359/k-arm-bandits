module KAB where

import Control.Applicative
import Control.Monad
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

testComp = "."

normalDist = normalDistr 0 1

numActions = 10

sampleNormal :: IO Double
sampleNormal = withSystemRandom (genContVar normalDist :: (GenIO -> IO Double))

actionMeans :: IO [Double]
actionMeans = replicateM numActions sampleNormal

sampleValue :: Double -> IO Double
sampleValue v = fmap (+ v) sampleNormal

sampleNonstationaryStep :: IO Double
sampleNonstationaryStep = fmap (/ 10) sampleNormal

stepActions :: [Double] -> [Double] -> [Double]
stepActions = zipWith (+)

progress :: [Double] -> Int -> IO (Double, [Double])
progress values move = (,) <$> (sampleValue $ values !! move) <*> (stepActions values <$> (replicateM numActions sampleNonstationaryStep))

type SampleAverageStats = [(Double, Int)]

observe :: SampleAverageStats -> Int -> Double -> SampleAverageStats
observe oldStats actionNum latestSample =
  if (length oldStats <= actionNum)
    then error $ "action number " ++ (show actionNum) ++ " does not exist in the list " ++ (show oldStats)
    else let (avg, count) = (oldStats !! actionNum) in take actionNum oldStats ++ ((avg + ((latestSample - avg) / (fromIntegral $ count + 1)), count + 1) : (drop (actionNum + 1) oldStats))

act :: SampleAverageStats -> Int
act stats = undefined