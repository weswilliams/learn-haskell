type FirstName = String
type LastName = String
type Age = Int
type Height = Float
type PhoneNumber = String
type Flavor = String

data Person = Person {
  firstName :: FirstName
, lastName :: LastName
, age :: Age
, height :: Height
, phoneNumber :: PhoneNumber
, flavor :: Flavor } deriving(Read)

instance Eq Person where
  (Person fn1 ln1 _ _ _ _) == (Person fn2 ln2 _ _ _ _) = (fn1 == fn2) && (ln1 == ln2)

instance Show Person where
  show (Person fn ln age ht pn fl) = fn ++ " " ++ ln ++ " is " ++ show age ++ " year(s) old"
 
instance Ord Person where
  compare (Person fn1 ln1 _ _ _ _) (Person fn2 ln2 _ _ _ _)
    | (fn1 == fn2) && (ln1 == ln2) = EQ
    | (ln1 < ln2) = LT
    | (ln1 > ln2) = GT
    | (fn1 < fn2) = LT
    | (fn1 > fn2) = GT
 
