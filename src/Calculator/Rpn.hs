module Calculator.Rpn (solveRPN) where

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (y * x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "+" = (y + x):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction xs numberString = read numberString:xs

--  "10 4 3 + 2 * -"
--  [] 10 -> [10]
--  [10] 4 -> [4, 10]
--  [4, 10] 3 -> [3, 4, 10]
--  [3,4,10] + -> [7, 10]
--  [7, 10] 2 -> [2,7,10]
--  [2,7,10] * -> [14,10]
--  [14,10] - -> [-4]
-- -4
