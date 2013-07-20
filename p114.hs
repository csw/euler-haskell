import qualified Data.MemoCombinators as Memo

placements :: Int -> Int
placements w = sum [placeN n w | n <- [0..maxN]]
  where maxN = (w+1) `div` 4

placeN :: Int -> Int -> Int
placeN = Memo.memo2 Memo.integral Memo.integral placeN'
  where
    placeN' 0 _ = 1
    placeN' 1 w
      | w < 3     = 0
      | otherwise = (w-2) + placeN 1 (w-1)
    placeN' n w
      | w < (4*n)-1 = 0
      | otherwise   = placeRest + placeN n (w-1)
      where placeRest = sum [placeN (n-1) w' | w' <- [(4*(n-1)-1)..(w-4)]]

main = do
  putStrLn $ show $ placements 50
