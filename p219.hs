import System.Environment

-- Project Euler #219, Skew-cost coding.
-- See http://projecteuler.net/problem=219 for details.

-- Note that I worked most of this problem out on paper; the code is
-- just doing the part I couldn't solve analytically, and doesn't
-- really stand alone very well.

-- TODO: document once I've got my notes in front of me. The basic
-- idea, though, is tracking the number (i.e. frequency) of prefix
-- code nodes with a given cost, for a minimal-cost code of a given
-- size. Then, conceptually split all minimal-cost nodes at once until
-- the target code size is reached.

type Freq = Integer

data FStep = Step Integer Integer Freq Freq Freq Freq
             deriving Show

seqStart :: FStep
seqStart = Step 2 1 1 0 0 1

minCost :: FStep -> Integer
minCost (Step c base f0 f1 f2 f3) =
  base*f0 + (base+1)*f1 + (base+2)*f2 + (base+3)*f3

findMinCost :: Integer -> Integer
findMinCost size = (minCost st) + delta*(base+5)
  where st@(Step c base f0 _ _ _) = lastFreqStepFor size seqStart
        delta = size - c

-- TODO: see about removing the explicit recursion for perhaps a more
-- Haskelly solution. This does seem like the obvious way to formulate
-- it, though.

lastFreqStepFor :: Integer -> FStep -> FStep
lastFreqStepFor target st@(Step c _ f0 _ _ _)
  | (c+f0) > target = st
  | otherwise       = lastFreqStepFor target $ freqStep st

freqStep :: FStep -> FStep
freqStep (Step c base f0 f1 f2 f3) = Step (c+f0) (base+1) (f0+f1) f2 f3 f0

main :: IO ()
main = do
  args <- getArgs
  print $ findMinCost $ read $ head args
