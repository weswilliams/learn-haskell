module Hello where

main :: IO()
main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines were: " ++ a
