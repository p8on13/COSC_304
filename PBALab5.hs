-- COSC 304 Fall 2018
-- Lab 5, October 4, 2018
-- PBALab5

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab5 where

{- Expressions test1, ... test8 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}


monext f bin e [] = e
monext f bin e (head:rest) = bin (f head, monext f bin e rest)

inlist val [] = False
inlist val (start:end) = if start==val
                         then True
                         else inlist val end

double x = 2*x

ndspec1 'a' 0 = [1, 2]
ndspec1 'a' 1 = [2, 3]
ndspec1 'a' 2 = [0, 3]
ndspec1 'a' 3 = [0, 1]
ndspec1 'b' 0 = [0]
ndspec1 'b' 1 = [1]
ndspec1 'b' 2 = [2]
ndspec1 'b' 3 = [3]

ndspec2 'a' 0 = [1, 2]
ndspec2 'a' 1 = []
ndspec2 'a' 2 = [3]
ndspec2 'a' 3 = []
ndspec2 'b' 0 = [3]
ndspec2 'b' 1 = [2]
ndspec2 'b' 2 = []
ndspec2 'b' 3 = []

nndspec3 "a" 0 = [1]
nndspec3 "a" 1 = []
nndspec3 "a" 2 = [3]
nndspec3 "a" 3 = []
nndspec3 "b" 0 = []
nndspec3 "b" 1 = [2]
nndspec3 "b" 2 = []
nndspec3 "b" 3 = [3]
nndspec3 "" 0 = []
nndspec3 "" 1 = []
nndspec3 "" 2 = [0]
nndspec3 "" 3 = []

noreplist [] = []
noreplist (head:rest) = if inlist head rest
                        then noreplist rest
                        else head:(noreplist rest)
minlist [x] = x
minlist (head:rest) = if head < minlist rest
                      then head
                      else minlist rest

sortlist [] = []
sortlist [x] = [x]
sortlist (head:rest) = if head < minlist(rest)
                       then head:(sortlist rest)
                       else sortlist (rest++[head])


fixlist list = sortlist (noreplist list)

flatten [] = []
flatten [x] = x
flatten (head:rest) = head++flatten rest

relcomp (realtion1, realtion2) val = fixlist (flatten(map realtion2 (realtion1 val)))

relidentity x = [x]
nfa ndspec = monext ndspec relcomp relidentity

nfa1 = nfa ndspec1

test1=nfa1 "abbababbaba" 1

startndm ndspec string = nfa ndspec string 0

test2=startndm ndspec1 "abbababbaba"

inter [] list2 = []
inter (head:rest) list2 = if inlist head list2
                          then head:(inter rest list2)
                          else inter rest list2

ndfa (ndspec, finalstates) string = not (inter (startndm ndspec string) finalstates == [])

test3= ndfa(ndspec1, [0,2]) "abbababbabaaaa"
test4= ndfa(ndspec1, [0]) "ab"
test5= ndfa(ndspec2, [2]) "a"
test6= ndfa(ndspec2, [2]) "abab"

nnfa nndspec = monext nndspec relcomp relidentity
nstartndm nndspec string = nnfa nndspec string 0
nndfa (nndspec, finalstates) string = not (inter (nstartndm nndspec string) finalstates == [])

test7= nstartndm nndspec3 ["a", "b", "", "a"] == nstartndm nndspec3 ["a", "b", "a"]
test8= nndfa(nndspec3, [2])["a", "b", "", "a", "b"]
