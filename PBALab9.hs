-- COSC 304 Fall 2018
-- Lab9 Nov 15, 2018
-- PBALab9

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab9 where

{- Expressions test1, ... test5 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}

spec1(0, '#') = (0, '#', 'l')
spec1(0, 'a') = (1, 'a', 'l')
spec1(0, 'b') = (1, 'a', 'l')
spec1(0, 'c') = (1, 'a', 'l')
spec1(0, 'd') = (1, 'a', 'l')

spec1(1, '#') = (2, '#', 'r')
spec1(1, 'a') = (1, 'a', 'l')
spec1(1, 'b') = (1, 'a', 'l')
spec1(1, 'c') = (1, 'a', 'l')
spec1(1, 'd') = (1, 'a', 'l')

spec1(2, '#') = (100, '#', 'd')
spec1(2, 'a') = (2, 'a', 'r')
spec1(2, 'b') = (2, 'b', 'r')
spec1(2, 'c') = (2, 'c', 'r')
spec1(2, 'd') = (2, 'd', 'r')

spec2(0, '#') = (1, '#', 'l')
spec2(0, 'a') = (0, 'a', 'l')
spec2(0, 'b') = (0, 'b', 'l')
spec2(0, 'c') = (0, 'c', 'l')
spec2(0, 'd') = (0, 'd', 'l')
spec2(1, '#') = (2, '#', 'r')
spec2(1, 'a') = (1, 'b', 'l')
spec2(1, 'b') = (1, 'c', 'l')
spec2(1, 'c') = (1, 'b', 'l')
spec2(1, 'd') = (1, 'b', 'l')
spec2(2, '#') = (100, '#', 'd')
spec2(2, 'a') = (2, 'a', 'r')
spec2(2, 'b') = (2, 'b', 'r')
spec2(2, 'c') = (2, 'c', 'r')
spec2(2, 'd') = (2, 'd', 'r')

spec3(0, '#') = (1, '#', 'l')
spec3(0, 'a') = (0, 'a', 'l')
spec3(0, 'b') = (0, 'b', 'l')
spec3(0, 'c') = (0, 'c', 'l')
spec3(0, 'd') = (0, 'd', 'l')

spec3(1, '#') = (2, '#', 'r')
spec3(1, 'a') = (1, 'd', 'l')
spec3(1, 'b') = (2, 'a', 'd')
spec3(1, 'c') = (1, 'd', 'l')
spec3(1, 'd') = (1, 'd', 'l')

spec3(2, '#') = (100, '#', 'd')
spec3(2, 'a') = (2, 'a', 'r')
spec3(2, 'b') = (2, 'b', 'r')
spec3(2, 'c') = (2, 'c', 'r')
spec3(2, 'd') = (2, 'd', 'r')

spec4(0, '#') = (100, 'a', 'r')
spec4(0,x) = (100, x, 'r')

listlength [] = 0
listlength (head:tail) = 1+listlength(tail)

val ((head:end), pos) = if pos == 0
                     then head
                     else val(end, pos-1)

startstring str = '#':str++"#"

startpos str = listlength(str)+1

str1 = "abcd"

chstr ((head:rest), ch, pos) = if pos ==0
                               then ch:rest
                               else head:chstr((rest), ch , pos-1)

chpos ch = if ch=='r' then 1
               else if ch == 'l' then -1
                    else 0

newmove tmspec (str, state, ch, pos) = let (newstate, newch, direction) = tmspec (state, ch) in
                             (chstr(str, newch, pos), newstate, val(chstr (str, ch, pos), pos + chpos (direction)), pos + chpos(direction))


newrun tmspec (str, state, ch, pos) = let (newString, newState, newCh, newPos) = newmove tmspec (str, state, ch, pos) in
                            if newState ==100 then (newString, newState, newCh, newPos)
                            else newrun tmspec (newString, newState, newCh, newPos)

newstartrun tmspec str = newrun tmspec (startstring str, 0, '#', startpos str)

newmove2 tmspec (str, state, ch, pos) = let (newstate, newch, direction) = tmspec (state, ch) in
                                        if (pos + chpos (direction)) == listlength (chstr (str, ch, pos))
                                        then (chstr (str, newch, pos) ++"#", newstate, val(chstr (str, ch, pos) ++"#", pos + chpos(direction)), pos + chpos(direction))
                                        else (chstr(str, newch, pos), newstate, val(chstr (str, ch, pos), pos + chpos (direction)), pos + chpos(direction))

newrun2 tmspec (str, state, ch, pos) = let (newString, newState, newCh, newPos) = newmove2 tmspec (str, state, ch, pos) in
                                      if newState ==100 then (newString, newState, newCh, newPos)
                                      else newrun2 tmspec (newString, newState, newCh, newPos)

newstartrun2 tmspec str = newrun2 tmspec (startstring str, 0, '#', startpos str)

--newstartrun spec4 'aaa' gives non exhaustive pattern fuction as we fall off the table as we end in an unacceptable state
test1 = newstartrun spec1 str1
test2 = newstartrun spec2 str1
test3 = newstartrun spec3 "ccad"
test4 = newstartrun2 spec4 "aaa"
test5 = newstartrun2 spec4 str1
