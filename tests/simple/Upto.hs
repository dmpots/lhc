-- RUN: %lhc -c %s 
-- RUN: %lhc compile %b.hcr
-- RUN: %b > %t1 ; diff %b.expected.stdout %t1
module Main where

main = putStrLn (show s)
  where s = m_sum (upto 1 10)


upto :: Integer -> Integer -> [Integer]
upto m n = if m > n then []
                    else m : upto (m+1) n
m_sum :: [Integer] -> Integer
m_sum l = case l of 
            []     -> 0
            (x:xs) -> x + sum xs
        
