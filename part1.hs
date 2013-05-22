-- P1: Sum of all multiple of 3 or 5 below 1000.

p1 = foldl (+) 0 [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- Sum of a finite arithmetic series
-- n elements, from a1 to an
p1_arithSum :: (Integral a) => a -> a -> a -> a
p1_arithSum n a1 an = (n * (a1 + an)) `div` 2

p1_arithSumTo :: (Integral a) => a -> a -> a
p1_arithSumTo n max =
  let maxMult = max `div` n
  in p1_arithSum maxMult n (n * maxMult)

-- sums of multiples of 3 and 5 in [1,1000)
-- minus multiples of 3*5 = 15 to avoid double-counting
p1a :: Integral a => a
p1a = (p1_arithSumTo 3 999) + (p1_arithSumTo 5 999) - (p1_arithSumTo 15 999)


-- P2: Find the sum of all the multiples of 3 or 5 below 1000.

p2 :: Int
p2 = sum [n | n <- takeWhile (< 4000000) p2_fib_seq, even n]

p2a :: Int
p2a = sum $ filter even $ takeWhile (< 4000000) p2_fib_seq

p2_fib :: Int -> Int -> [Int]
p2_fib a b = a : p2_fib b (a + b)

p2_fib_seq :: [Int]
p2_fib_seq = p2_fib 1 2

