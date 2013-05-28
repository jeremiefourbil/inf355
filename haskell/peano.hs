module Peano where

import Test.QuickCheck

data Peano = Zero | Succ Peano deriving (Read, Eq, Ord)

instance Num Peano where
  -- '-'
  a + Zero = a
  a + Succ b = Succ (a + b)

  -- '-'
  a - Zero = a
  Zero - a = 99999999 -- anything here in order to get all cases
  (Succ a) - (Succ b) = a - b
  
  -- '*'
  a * Zero = Zero
  a * Succ b = a + a * b

  

  -- signum
  signum Zero = 0
  signum (Succ _) = Succ Zero

  -- abs
  abs = id

  -- fromInteger
  fromInteger n | n < 0 = error "Peano numbers are always non-negative"
                | n == 0 = Zero
                | otherwise  = Succ (fromInteger (n - 1))

-- Show Peano where
instance Show Peano where
  show Zero = "Zero"
  show (Succ a) = "Succ(" ++ (show a) ++ ")"

instance Enum Peano where
  pred Zero = error "Zero has no predecessor"
  pred (Succ a) = a
  succ = Succ
  
  toEnum n | n < 0 = error "Peano numbers are always non-negative"
           | n == 0 = Zero
           | otherwise = Succ ( toEnum ( n - 1 ) )
  
  fromEnum Zero = 0
  fromEnum (Succ a) = 1 + (fromEnum a)

instance Integral Peano where
  toInteger Zero = 0
  toInteger (Succ a) = 1 + (toInteger  a)
  quotRem a b = ( fromInteger ((toInteger a) `div` (toInteger b)), fromInteger ((toInteger a) `mod` (toInteger b)))

instance Real Peano where
  toRational Zero = 0
  toRational (Succ a) = 1 + (toRational a)

instance Arbitrary Peano where
  arbitrary = do
    randomnumber <- choose (0, 100)
    return $ fromInteger(randomnumber)
