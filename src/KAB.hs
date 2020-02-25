module KAB where

import Control.Applicative
import Control.Monad
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

testComp = "."

normalDist = normalDistr 0 1

numActions = 10

runLength = 10000

alpha = 0.1
epsilon = 0.1

valueBias = 0.0

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

type AlphaValEstimates = [Double]

observeSA :: SampleAverageStats -> Int -> Double -> SampleAverageStats
observeSA oldStats actionNum latestSample =
  if (length oldStats <= actionNum)
    then error $ "action number " ++ (show actionNum) ++ " does not exist in the list " ++ (show oldStats)
    else let (avg, count) = (oldStats !! actionNum) in take actionNum oldStats ++ ((avg + ((latestSample - avg) / (fromIntegral $ count + 1)), count + 1) : (drop (actionNum + 1) oldStats))

observeAlp :: AlphaValEstimates -> Int -> Double -> AlphaValEstimates
observeAlp oldStats actionNum latestSample =
  if (length oldStats <= actionNum)
    then error $ "observeAlp does not contain index " ++ (show actionNum)
    else let oldAvg = (oldStats !! actionNum) in take actionNum oldStats ++ (oldAvg + (alpha * (latestSample - oldAvg)) : (drop (actionNum + 1) oldStats))

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f (a:as) = go a (f a) as
  where
    go amx _ [] = amx
    go amx maxSoFar (b:bs) = if (f b > maxSoFar)
      then go b (f b) bs
      else go amx maxSoFar bs

actGreedySA :: SampleAverageStats -> Int
actGreedySA stats = argmax (saValue stats) [0 .. (length stats)]

actGreedyAlpha :: AlphaValEstimates -> Int
actGreedyAlpha stats = argmax ((!!) stats) [0 .. (length stats)]

saValue :: SampleAverageStats -> Int -> Double
saValue stats actionNum = fst $ stats !! actionNum

initialValsAlp :: AlphaValEstimates
initialValsAlp = take numActions $ repeat valueBias

initialValsSA :: SampleAverageStats
initialValsSA = take numActions $ repeat (valueBias, 0)