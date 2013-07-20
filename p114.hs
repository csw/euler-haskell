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

-- #115: generalized with a parameterized minimum length

solve115 = threshold115 50 1000000

threshold115 :: Int -> Int -> Int
threshold115 m t = snd $ head $ dropWhile ((< t) . fst) allP
  where allP = [(placements115 m n, n) | n <- [0..]]

placements115 :: Int -> Int -> Int
placements115 m w = sum [placeMN m n w | n <- [0..maxN]]
  where maxN = (w+1) `div` 4

placeMN :: Int -> Int -> Int -> Int
placeMN = Memo.memo3 Memo.integral Memo.integral Memo.integral placeMN'
  where
    placeMN' m 0 _ = 1
    placeMN' m 1 w
      | w < m     = 0
      | otherwise = (w-m+1) + placeMN m 1 (w-1)
    placeMN' m n w
      | w < (minBuf*n)-1 = 0
      | otherwise   = placeRest + placeMN m n (w-1)
      where placeRest =
              sum [placeMN m (n-1) w' | w' <- [minRemain..maxRemain]]
            minRemain = (minBuf*(n-1)-1)
            maxRemain = (w-minBuf)
            minBuf = m+1
