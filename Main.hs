module Main where

main :: IO ()
main = do
  mapM_ (putStrLn . showRecip) [2..100]

reciprocal :: Int -> (String, Int)
reciprocal n | n > 1 = ('0' : '.' : digits, recur)
  where
    (digits, recur) = divide n 1 []

--
--
--
-- This is a very clever bit of code. It relies on a bit of theory
-- that all rational numbers have decimal expansions with finite length
-- cycles. This code detects those cycles.
--
-- Notice that `n` does not change in the recursive call to `divide`
-- but that `cs` is extended. This way we can detect when we've
-- "got into a loop". This means we discovered the end of a cycle
--
divide :: Int -> Int -> [Int] -> (String, Int)
divide n c cs | c `elem` cs = ([], position c cs)
              | r == 0      = (show q, 0)
              | r /= 0      = (show q ++ digits, recur)
  where
    (q, r) = (c*10) `quotRem` n
    (digits, recur) = divide n r (c:cs)


{-

     divide 11 1 []
                     (q,r) = (0,10)
  == (show 0 ++ digits1, recur1)
    where
      (digits1, recur1) = divide 11 10 [1]
                                        (q,r) = (9,1)
      == (show 9 ++ digits2, recur2)
        where
          (digits2, recur2) = divide 11 1 [10,1]
           -- "c `elem` cs" is True here
        == ([], position 1 [10,1])
        == ([], 2)
      == (show 9 ++ [], 2)
  == (show 0 ++ show 9 ++ [], 2)
  == ("09", 2)

-}

position :: Int -> [Int] -> Int
position n (x:xs) | n == x    = 1
                  | otherwise = 1 + position n xs

showRecip :: Int -> String
showRecip n =
    "1/" ++ show n ++ " = " ++
    if r == 0 then d else take p d ++ "(" ++ drop p d ++ ")"
  where
    p = length d - r
    (d, r) = reciprocal n


