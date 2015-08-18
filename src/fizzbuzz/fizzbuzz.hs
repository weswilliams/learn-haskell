module Fizzbuzz.Fizzbuzz (fizzBuzz) where

if' :: Bool -> a -> a -> a
if' True thenValue _  = thenValue
if' False _ elseValue = elseValue

isDivisibleBy :: (Integral a) => a -> a -> Bool
isDivisibleBy divisor = (0 ==) . (`mod` divisor)

isFizz = isDivisibleBy 3

isBuzz = isDivisibleBy 5

fizz n = if' (isFizz n) "Fizz" ""
buzz n = if' (isBuzz n) "Buzz" ""

fizzAndOrBuzz :: Int -> String
fizzAndOrBuzz n
  | or [isFizz n, isBuzz n] = fizz n ++ buzz n
  | otherwise = show n

fizzBuzz :: [Int] -> String
fizzBuzz nums = foldl (\acc n -> acc ++ (fizzAndOrBuzz n)) "" nums