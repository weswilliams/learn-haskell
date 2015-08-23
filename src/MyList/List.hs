module MyList.List where

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving(Show, Read, Eq, Ord)

instance Functor List where
  fmap f Empty = Empty
  fmap f (x:-:xs) = (f x) :-: (fmap f xs)