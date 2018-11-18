-- COSC 304 Fall 2018
-- Lab 2, Sept 13, 2018
-- ABCLab2

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab2 where

listlength [] = 0
listlength (head:tail) = 1+length(tail)
listsum [] = 0
listsum (head:tail) = head+listsum(tail)

data Nat = Z | S Nat deriving (Read, Show, Eq)

add nat1 Z = nat1
add nat1 (S nat2) = S ( add nat1 nat2)

one = S(Z)
two = S(S(Z))
three = S(two)
four = S(three)

minus nat1 Z = nat1
minus (S nat1) (S nat2)= minus nat1 nat2

mult nat1 Z = Z
mult nat1 (S Z) = nat1
mult nat1 (S nat2) = add nat1 (mult nat1 nat2)

nattoInt Z = 0
nattoInt (S nat1) = 1 + nattoInt(nat1)

buildNat 0 = Z
buildNat x = S(buildNat (x-1))

factNat Z = Z
factNat (S nat1) = mult (S nat1) (factNat nat1)

addA [] = []
addA (val:rest) = (val ++ ['a']):addA rest

astar = "":addA(astar)

lt Z Z = False
lt nat Z = False
lt Z nat = True
lt (S nat1) (S nat2) = lt nat1 nat2
lte Z Z = True
lte nat Z = False
lte Z nat = True
lte (S nat1) (S nat2) = lte nat1 nat2

test1 = add (buildNat 3) (buildNat 5)
test2 = lte(buildNat 3) (buildNat 5)
test3 = nattoInt(factNat(buildNat 5))
test4 = take 6 astar

{-mult nat1 (S nat2) = mult (add nat1 nat1) nat2
exponential

 -}
