-- COSC 304 Fall 2018
-- Lab6 Oct 25, 2018
-- PBALab6

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab6 where

monext f bin e [] = e
monext f bin e (head:rest) = bin (f head, monext f bin e rest)

inlist val [] = False
inlist val (start:end) = if start==val
                         then True
                         else inlist val end

comp(a,b) input = b(a input)

first(x, y) = x
second (x, y)=y
identity x = x

spec1 'a' (0,stack) = (0, 'a':(second(0, stack)))
spec1 'b' (0, stack) = (0, 'b':(second(0, stack)))
spec1 'c' (0, stack) = (1, stack)
spec1 'a' (1, start:stack) = if start == 'a' then (1, stack)
                             else (1, start:stack)
spec1 'b' (1, start:stack) = if start == 'b' then (1, stack)
                             else (1, start:stack)

spec2 'x' (0, "") = (1, "c")
spec2 'a' (1, 'c':stack) = (1, "ac"++stack)
spec2 'a' (1, 'a':stack) = (1, "aa"++stack)
spec2 'a' (1, 'b':stack) = (1, stack)
spec2 'b' (1, 'c':stack) = (1, "bc"++stack)
spec2 'b' (1, 'a':stack) = (1, "ba"++stack)
spec2 'b' (1, 'b':stack) = (1, stack)
spec2 'z' (1, "c") = (2, "")
spec2 'z' feed = feed
final2 = [2]

pdaspec2 = (spec2, final2)
pda2 = pdacreate pdaspec2

final1 = [1]

pdaspec1 = (spec1, final1)

pdacreate pdaspec = (monext (first pdaspec) comp identity, second pdaspec)

pdacomp pda string = (first pda) string (0, "")

pda1 = pdacreate pdaspec1

test1 = pdacomp pda1 "abbacab"

pdatest pda string = inlist (first (pdacomp pda string)) (second pda) && second(pdacomp pda string)==""

test2 = pdatest pda1 "abbcbba"
test3 = pdacomp pda2 "xaabbabaz"
test4 = pdatest pda2 "xaabbabaz"


{- Expressions test1, ... test4 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}
