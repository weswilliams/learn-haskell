module Calculator.Calc (reduce, by, Reduction(Exactly,Percent,Fractionally)) where

data Reduction = Exactly | Percent | Fractionally

reduce :: Fractional a => a -> (a -> Reduction -> a) -> a -> Reduction -> a
reduce a f b Exactly = a - b
reduce a f b Percent = (a -) $ (a *) $ f b Percent
reduce a f b Fractionally = (a -) $ a * b

by :: (Fractional a) => a -> Reduction -> a
by a Exactly = a
by a Percent = a / 100
