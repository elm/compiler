
module List where

import Native.List as L

head : [a] -> a
head = L.head

tail : [a] -> [a]
tail = L.tail

last : [a] -> a
last = L.last

map : (a -> b) -> [a] -> [b]
map = L.map

foldl : (a -> b -> b) -> b -> [a] -> b
foldl = L.foldl

foldr : (a -> b -> b) -> b -> [a] -> b
foldr = L.foldr

foldl1 : (a -> a -> a) -> [a] -> a
foldl1 = L.foldl1

foldr1 : (a -> a -> a) -> [a] -> a
foldr1 = L.foldr1

scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl = L.scanl

scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 = L.scanl1

filter : (a -> Bool) -> [a] -> [a]
filter = L.filter

length : [a] -> Int
length = L.length

reverse : [a] -> [a]
reverse = L.reverse

concat : [[a]] -> [a]
concat = L.foldr (++) []

concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = concat . L.map f

all : (a -> Bool) -> [a] -> Bool
all pred = L.all

any : (a -> Bool) -> [a] -> Bool
any = L.any

and : [Bool] -> Bool
and = L.all id

or : [Bool] -> Bool
or = L.any id

sum = foldl (+) 0
product = foldl (*) 1
maximum = foldl1 max
minimum = foldl1 min

partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred lst =
    case lst of
      []    -> ([],[])
      x::xs -> let (bs,cs) = partition pred xs in
               if pred x then (x::bs,cs) else (bs,x::cs)

zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = L.zipWith

zip : [a] -> [b] -> [(a,b)]
zip = L.zip

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
