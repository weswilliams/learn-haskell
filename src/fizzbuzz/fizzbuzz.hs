module Fizzbuzz.Fizzbuzz (fizzBuzz) where

data CardValue = Fizz | Buzz
  deriving (Read,  Show, Enum, Eq, Ord)

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy dividend divisor = 0 == (dividend `mod` divisor)

fizzAndOrBuzz :: Int -> String
fizzAndOrBuzz n
  | n `isDivisibleBy` 15 = show Fizz ++ show Buzz
  | n `isDivisibleBy` 5 = show Buzz
  | n `isDivisibleBy` 3 = show Fizz
  | otherwise = show n

fizzBuzz :: [Int] -> String
fizzBuzz nums = foldl (\acc n -> acc ++ (fizzAndOrBuzz n)) "" nums