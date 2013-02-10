
module List where

import Native.List as Native

head : [a] -> a
head = Native.head

tail : [a] -> [a]
tail = Native.tail

last : [a] -> a
last = Native.last

map : (a -> b) -> [a] -> [b]
map = Native.map

foldl : (a -> b -> b) -> b -> [a] -> b
foldl = Native.foldl

foldr : (a -> b -> b) -> b -> [a] -> b
foldr = Native.foldr

foldl1 : (a -> a -> a) -> [a] -> a
foldl1 = Native.foldl1

foldr1 : (a -> a -> a) -> [a] -> a
foldr1 = Native.foldr1

scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl = Native.scanl

scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 = Native.scanl1

filter : (a -> Bool) -> [a] -> [a]
filter = Native.filter

length : [a] -> Int
length = Native.length

reverse : [a] -> [a]
reverse = Native.reverse

concat : [[a]] -> [a]
concat = Native.foldr (++) []

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = concat . Native.map f

all : (a -> Bool) -> [a] -> Bool
all pred = Native.all

any : (a -> Bool) -> [a] -> Bool
any = Native.any

and : [Bool] -> Bool
and = Native.all id

or : [Bool] -> Bool
or = Native.any id

sum = foldl (+) 0
product = foldl (*) 1
maximum = foldl1 max
minimum = foldl1 min

partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred lst =
    case lst of
      []    -> ([],[])
      x::xs -> let (as,bs) = partition pred xs  in
               if pred x then (x::as,bs) else (as,x::bs)

zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = Native.zipWith

zip : [a] -> [b] -> [(a,b)]
zip = Native.zip

unzip : [(a,b)] -> ([a],[b])
unzip pairs =
  case pairs of
    []        -> ([],[])
    (x,y)::ps -> let (xs,ys) = (unzip ps) in (x::xs,y::ys)

intersperse sep xs =
  case xs of 
    a::b::cs -> a :: sep :: intersperse sep (b::cs)
    [a] -> [a]
    []  -> []

intercalate sep xs =
  case xs of 
    a::b::cs -> a ++ sep ++ intercalate sep (b::cs)
    [a] -> a
    []  -> []
