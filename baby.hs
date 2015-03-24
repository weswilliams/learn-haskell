keepUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

lucky :: Int -> String
lucky 7 = "lucky number seven"
lucky x = show x ++ " is not lucky"

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEq = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEq ++ [x] ++ quicksort larger
 
fib :: Integer -> Integer -> [Integer]
fib a b = a:fib b (a+b)

fibs = fib 0 1

fibsFromElem :: Int -> [Integer]
fibsFromElem n
  | n <= 0 = fibs
fibsFromElem n = fib (fibs !! n) (fibs !! (n+1))

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "no values"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "must be non empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty list"
tell (x:[]) = "one element list: " ++ show x
tell (x:y:[]) = "two element list: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "too many element list begins with: " ++ show x ++ ", " ++ show y

fstLetter :: String -> String
fstLetter "" = "empty string"
fstLetter all@(x:remainder) = all ++ " begins with " ++ [x] ++ ", remainder is " ++ remainder

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "overweight"
  | otherwise = "obese"
  where bmi = weight / height ^ 2

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w/h^2]

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea


