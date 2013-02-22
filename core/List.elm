
module List where

import Native.List

head : [a] -> a
head = Native.List.head

tail : [a] -> [a]
tail = Native.List.tail

last : [a] -> a
last = Native.List.last

map : (a -> b) -> [a] -> [b]
map = Native.List.map

foldl : (a -> b -> b) -> b -> [a] -> b
foldl = Native.List.foldl

foldr : (a -> b -> b) -> b -> [a] -> b
foldr = Native.List.foldr

foldl1 : (a -> a -> a) -> [a] -> a
foldl1 = Native.List.foldl1

foldr1 : (a -> a -> a) -> [a] -> a
foldr1 = Native.List.foldr1

scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl = Native.List.scanl

scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 = Native.List.scanl1

filter : (a -> Bool) -> [a] -> [a]
filter = Native.List.filter

length : [a] -> Int
length = Native.List.length

reverse : [a] -> [a]
reverse = Native.List.reverse

concat : [[a]] -> [a]
concat = Native.List.foldr (++) []

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = concat . Native.List.map f

all : (a -> Bool) -> [a] -> Bool
all pred = Native.List.all

any : (a -> Bool) -> [a] -> Bool
any = Native.List.any

and : [Bool] -> Bool
and = Native.List.all id

or : [Bool] -> Bool
or = Native.List.any id

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
zipWith = Native.List.zipWith

zip : [a] -> [b] -> [(a,b)]
zip = Native.List.zip

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
