-- COSC 304 Fall 2018
-- Lab 1, Sept 6, 2018
-- PBALab1 

module PBALab1 where

double x = 2*x

triple x = 3*x

add1 (x, y) = x+y

add2 x y = x+y

fact(0)=1
fact (x)= x*fact(x-1)

boolval1= 3==4

first(x, y) = x
second (x, y)=y

listlength(x) = length x

test1 = listlength[listlength[2,3,4,5,6]] 
test2 = second(add1,fact)(test1+4)

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

