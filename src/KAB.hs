module KAB where

import Control.Applicative
import Control.Monad
import Statistics.Distribution (genContVar)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform (uniformDistr)
import System.Random.MWC (withSystemRandom, GenIO)

testComp = "."

normalDist = normalDistr 0 1
uniformDist = uniformDistr 0 1

numActions = 10

runLength = 10000

alpha = 0.1
epsilon = 0.1

valueBias = 0.0

type ActionMeans = [Double]

sampleNormal :: IO Double
sampleNormal = withSystemRandom (genContVar normalDist :: (GenIO -> IO Double))

actionMeans :: IO ActionMeans
actionMeans = replicateM numActions sampleNormal

sampleValue :: Double -> IO Double
sampleValue v = fmap (+ v) sampleNormal

sampleNonstationaryStep :: IO Double
sampleNonstationaryStep = fmap (/ 100) sampleNormal

stepValues :: [Double] -> IO [Double]
stepValues values = sequenceA $ fmap takeStep values
  where
    takeStep val = do
      step <- sampleNonstationaryStep
      return $ val + step


--progress :: [Double] -> Int -> IO (Double, [Double])
--progress values move = (,) <$> (sampleValue $ values !! move) <*> (stepActions values <$> (replicateM numActions sampleNonstationaryStep))

type SampleAverageStats = [(Double, Int)]

type AlphaValEstimates = [Double]

getReward :: ActionMeans -> Int -> IO Double
getReward means actionNum = 
  if (length means <= actionNum)
    then error $ "getReward: index " ++ (show actionNum) ++" is too large for list of length " ++ (show $ length means)
    else ((+) (means !! actionNum)) <$> sampleNormal

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
actGreedySA stats = argmax (saValue stats) [0 .. (length stats - 1)]

actGreedyAlpha :: AlphaValEstimates -> Int
actGreedyAlpha stats = argmax ((!!) stats) [0 .. (length stats - 1)]

actEpsilonGreedy :: Int -> IO Int
actEpsilonGreedy greedyChoice = do
  randomSample <- withSystemRandom (genContVar uniformDist :: (GenIO -> IO Double))
  if (randomSample <= epsilon)
    then randomAction
    else return greedyChoice

randomAction :: IO Int
randomAction = do
  randomSample <- withSystemRandom (genContVar uniformDist :: (GenIO -> IO Double))
  return $ floor $ randomSample * (fromIntegral numActions)


saValue :: SampleAverageStats -> Int -> Double
saValue stats actionNum =
  if (length stats <= actionNum)
    then error "saValue index too large"
    else fst $ stats !! actionNum

initialValsAlp :: AlphaValEstimates
initialValsAlp = take numActions $ repeat valueBias

initialValsSA :: SampleAverageStats
initialValsSA = take numActions $ repeat (valueBias, 0)

initializeScenario :: IO (AlphaValEstimates, SampleAverageStats, ActionMeans)
initializeScenario = (,,) initialValsAlp initialValsSA <$> actionMeans

runScenario :: IO [[Double]]
runScenario = go runLength initialValsAlp initialValsSA =<< actionMeans
  where
    go :: Int -> AlphaValEstimates -> SampleAverageStats -> ActionMeans -> IO [[Double]]
    go 0 _ _ _ = return []
    go n alphaEsts averageEsts means = do
      --putStrLn $ "Time Remaining: " ++ (show n)

      alphaAction <- actEpsilonGreedy $ actGreedyAlpha alphaEsts
      alphaReward <- getReward means alphaAction

      saAction <- actEpsilonGreedy $ actGreedySA averageEsts
      saReward <- getReward means saAction

      newMeans <- stepValues means

      ((:) [alphaReward, saReward]) <$> (go (n - 1) (observeAlp alphaEsts alphaAction alphaReward) (observeSA averageEsts saAction saReward) newMeans)

average :: [[Double]] -> [Double]
average nums = fmap (flip (/) (fromIntegral $ length nums)) $ foldr (zipWith (+)) (head nums) (tail nums)

reportPerformance :: IO [Double]
reportPerformance = fmap average runScenario