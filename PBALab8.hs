-- COSC 304 Fall 2018
-- Lab8 Nov 8, 2018
-- ABCLab8

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module ABCLab8 where


{- Expressions test1, ... test6 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}

spec1 (0, '#') = (0, '#', 'l')
spec1(0, 'a') = (1, 'a', 'l')
spec1(0, 'b') = (1, 'a', 'l')
spec1(0, 'c') = (1, 'a', 'l')
spec1(0, 'd') = (1, 'a', 'l')

spec1(1, '#') = (2, '#', 'r')
spec1(1, 'a') = (2, 'a', 'l')
spec1(1, 'b') = (2, 'a', 'l')
spec1(1, 'c') = (2, 'a', 'l')
spec1(1, 'd') = (2, 'a', 'l')

spec1(2, '#') = (100, '#', 'd')
spec1(2, 'a') = (2, 'a', 'r')
spec1(2, 'b') = (2, 'b', 'r')
spec1(2, 'c') = (2, 'c', 'r')
spec1(2, 'd') = (2, 'd', 'r')

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

move(str, state, ch, pos) = let (newstate, newch, direction) = spec1 (state, ch) in
                             (chstr(str, newch, pos), newstate, val(chstr (str, ch, pos), pos + chpos (direction)), pos + chpos(direction))

test1 = move(str1, 0 , 'd', 3)
test2 = move(str1, 1, 'c', 2)
test3 = move(move(str1, 0 , 'd', 3))

run (str, state, ch, pos) = let (newString, newState, newCh, newPos) = move(str, state, ch, pos) in
                               if newState ==100 then (newString, newState, newCh, newPos)
                               else run(newString, newState, newCh, newPos)

startrun str = run(startstring str, 0, '#', startpos str)

test4 = startrun str1
test5 = startrun "bbccaa"
test6 = startrun "bb#cd"
