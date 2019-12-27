-- My complex number class

instance (Show a) => Show (Complex a) where
  show (C x y) = "C " ++ show x ++ " " ++ show y
  
instance (Eq a) => Eq (Complex a) where
  (C a ai) == (C b bi) = a == b && ai == bi
  
instance (Eq a, Ord a) => Ord (Complex a) where
  compare (C a ai) (C b bi) | a < b || a == b && ai < bi = LT
                            | a > b || a == b && ai > bi = GT
                            | otherwise = EQ

-- Note that we are using a plane where imaginary (y-axis) numbers
-- increase downward. I think this means that right and left are
-- reversed.

data Complex a = C a a

-- instance Num (Complex Int) where
--   abs c = error "Can't take abs of Complex Int"

instance Num a => Num (Complex a) where
  (C x y) + (C u v) = C (x+u) (y+v)
  (C x y) * (C u v) = C (x*u-y*v) (x*v+y*u)
  fromInteger n = C (fromInteger n) 0
  abs (C x y) = C (abs $ x + y) 0 -- manhattan distance -- sqrt $ x*x + y*y
  signum (C x y) = C (signum x) 0
  negate (C x y) = C (negate x) (negate y)
