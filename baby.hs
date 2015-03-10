lucky :: Int -> String
lucky 7 = "lucky number seven"
lucky x = show x ++ " is not lucky"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
fstLetter all@(x:xs) = all ++ " begins with " ++ [x]

