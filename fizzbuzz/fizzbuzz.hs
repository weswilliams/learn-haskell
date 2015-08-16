isFizz :: (Integral a) => a -> Bool
isFizz = (0 ==) . (`mod` 3)

isBuzz :: (Integral a) => a -> Bool
isBuzz n = mod n 5 == 0

fizzAndOrBuzz :: Int -> String
fizzAndOrBuzz n
  | and [isFizz n, isBuzz n] = "FizzBuzz"
  | isFizz n  = "Fizz"
  | isBuzz n  = "Buzz"
  | otherwise = show n

fizzBuzz :: [Int] -> String
fizzBuzz nums = foldl (\acc n -> acc ++ (fizzAndOrBuzz n)) "" nums