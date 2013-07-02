#!/usr/bin/env runghc
 {-# LANGUAGE UnicodeSyntax #-}
data Matrix = Matrix {value::[Int]} deriving (Show)

-- value::Matrix -> [Int]
-- value (Matrix a) = a
m0 = Matrix {value=[1,2,3]}
m1 = Matrix {value=[1,2,3]}

-- v::[Int]
-- v = value m0
-- a = zipWith(+) value m0 value m1
-- a = zipWith(+) (value m0) (value m1)

sum_ :: Int -> a -> Int -> (Int->Int) -> Int
     
sum_ a _ b f = foldr (+) 0 (map (f) [b..a])

(∑) = sum_
i_eq = 0
-- (ᵢ₌) = i_eq 
(ᵢ) = i_eq 

-- t= sum_ 3 i_eq 1  (\x -> 2*x+1)
-- t = ∑3ᵢ₌1 (\x -> 2*x+1)
-- t = (∑) 3 (ᵢ₌) 1 (\x -> 2*x+1)
t = (∑) 3 (ᵢ) 1 (\x -> 2*x+1)

class Op a where
      (⊕) :: a -> a -> a
      (⊗) :: a -> a -> a
instance Op Matrix where
       a ⊕ b   =Matrix {value=(zipWith (+) (value a) (value b))}
       a ⊗ b   =Matrix {value=(zipWith (*) (value a) (value b))}

main=do
        print( t )
        print( m0 ⊗ m1 )
