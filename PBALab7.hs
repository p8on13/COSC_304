-- COSC 304 Fall 2018 
-- Lab6 Oct 25, 2018
-- PBA Lab6 

{- Replace ABC above and below with your 3 letters, capped.
Then rename the file, again changing ABC with your 3 letters, capped. -}

module PBALab6 where 


{- Expressions test1, ... test4 ARE DEFINED IN THIS FILE, NOT IN THE TEST FILE -}

monext f bin e [] = e
monext f bin e (head:rest) = bin (f head, monext f bin e rest)

flatten [] = []
flatten [x] = x
flatten (head:rest) = head++flatten rest

relcomp (realtion1, realtion2) val = flatten(map realtion2 (realtion1 val))

relidentity x = [x]

inlist val [] = False
inlist val (start:end) = if start==val
                         then True
                         else inlist val end


first(x, y) = x
second (x, y)=y
identity x = x

gspec1 0 'S' = "aSb"
gspec1 0 ch = [ch]
gspec1 1 'S' = ""
gspec1 1 ch = [ch]

gspec2 0 'S' = "aSa"
gspec2 0 ch = [ch]
gspec2 1 'S' = "bSb"
gspec2 1 ch = [ch]
gspec2 2 'S' = ""
gspec2 2 ch = [ch]

gspec3 0 'E' = "E+T"
gspec3 0 ch = [ch]
gspec3 1 'E' = "T"
gspec3 1 ch = [ch]
gspec3 2 'T' = "x*T"
gspec3 2 ch = [ch]
gspec3 3 'T' = "x"
gspec3 3 ch = [ch]

gspec4 0 'S' = "aSb"
gspec4 0 ch = [ch]
gspec4 1 'S' = "bSa"
gspec4 1 ch = [ch]
gspec4 2 'S' = "c"
gspec4 2 ch = [ch]



buildgram gspec list ch = monext gspec relcomp relidentity list ch

gramgen1 = (buildgram gspec1, 'S')
gramgen2 = (buildgram gspec2, 'S')
gramgen3 = (buildgram gspec3, 'E')

startgramgen gramgen rulelist = (first gramgen) rulelist (second gramgen)

test1 = startgramgen gramgen1 [0,0,0,1]
test2 = startgramgen gramgen2 [0,0,1,0,2,2,1,2]
test3 = startgramgen gramgen3 [0,1,2,3,0,1]
test4 = startgramgen gramgen3 [3,3,0,1,2,1]

add0 [] = []
add0 (head:rest) = (head++[0]):(add0 rest)

star0 = []:add0 (star0)

add01 [] = []
add01 (head:rest) = (head ++ [0]) : (head ++ [1]) : (add01 rest)
star01 = []:add01(star01)

add012 [] = []
add012 (head:rest) = (head ++ [0]) : (head ++ [1]): (head ++ [2]) : (add012 rest)
star012 = []:add012(star012)

add0123 [] = []
add0123 (head:rest) = (head ++ [0]) : (head ++ [1]): (head ++ [2]) : (head++ [3]) : (add0123 rest)
star0123 = []:add0123(star0123)

testrun0123 gramgen n = take n  (map (startgramgen gramgen) star0123)



test5 = testrun0123 gramgen3 20

inLgramgen3 str n = inlist str (testrun0123 gramgen3 n)

test6 = inLgramgen3 "x+x*x" 400
test7 = inLgramgen3 "x*+x" 400
test8 = inLgramgen3 "x*x+x" 800

