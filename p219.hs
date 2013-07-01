import System.Environment

type Freq = Int

data FStep = Step Int Int Freq Freq Freq Freq
             deriving Show

seqStart :: FStep
seqStart = Step 2 1 1 0 0 1

minCost :: FStep -> Int
minCost (Step c base f0 f1 f2 f3) =
  base*f0 + (base+1)*f1 + (base+2)*f2 + (base+3)*f3

findMinCost :: Int -> Int
findMinCost size = (minCost st) + delta*(base+5)
  where st@(Step c base f0 _ _ _) = lastFreqStepFor size seqStart
        delta = size - c

lastFreqStepFor :: Int -> FStep -> FStep
lastFreqStepFor target st@(Step c _ f0 _ _ _)
  | (c+f0) > target = st
  | otherwise       = lastFreqStepFor target $ freqStep st

freqStep :: FStep -> FStep
freqStep (Step c base f0 f1 f2 f3) = Step (c+f0) (base+1) (f0+f1) f2 f3 f0

main :: IO ()
main = do
  args <- getArgs
  print $ findMinCost $ read $ head args
