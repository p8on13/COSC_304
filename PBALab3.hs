-- COSC 304 Fall 2018
-- Lab 3, Sept 20
-- PBALab3

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab3 where

{-
Expressions test1, ... test5 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}

add (x, y) = x+y
mult (x, y) = x*y

f n = 3*n+2
g n = n+1

monext f bin e [] = e
monext f bin e (head:rest) = bin (f head, monext f bin e rest)

faspec1 'a' 0 = 1
faspec1 'a' 1 = 2
faspec1 'a' 2 = 3
faspec1 'a' 3 = 0
faspec1 'b' 0 = 0
faspec1 'b' 1 = 1
faspec1 'b' 2 = 2
faspec1 'b' 3 = 3


comp(a,b) input = b(a input)

fa faspec = monext faspec comp id
fa1 = fa faspec1

startm faspec str = fa faspec str 0

dfa (faspec, finalstates) str = inlist (startm faspec str) finalstates

inlist val [] = False
inlist val (start:end) = if start==val
                         then True
                         else inlist val end

dfaspec1 = (faspec1, [0,2])
dfa1= dfa dfaspec1

test1 = fa1 "abbababbaba" 0
test2 = fa1 "abbababbababba" 1
test3 = startm faspec1 "abbababbababba"
test4 = dfa1 "abbababbaba"
test5 = dfa1 "abbababbababba"
