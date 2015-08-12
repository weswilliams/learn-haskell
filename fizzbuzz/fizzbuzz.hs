isFizz :: Int -> Bool
isFizz n = mod n 3 == 0

isBuzz :: Int -> Bool
isBuzz n = mod n 5 == 0

fizzBuzz :: Int -> String
fizzBuzz n
  | and [isFizz n, isBuzz n] = "FizzBuzz"
  | isFizz n  = "Fizz"
  | isBuzz n  = "Buzz"
  | otherwise = show n

fizzBuzzes :: [Int] -> String
fizzBuzzes nums = foldl (\acc n -> acc ++ (fizzBuzz n)) "" nums