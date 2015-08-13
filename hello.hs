main = do
  putStrLn "hello, what is your name?"
  name <- getLine
  putStrLn ("hello " ++ name)