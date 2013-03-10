
module List where

import Native.List as L

head : [a] -> a
tail : [a] -> [a]
last : [a] -> a
map  : (a -> b) -> [a] -> [b]
foldl  : (a -> b -> b) -> b -> [a] -> b
foldr  : (a -> b -> b) -> b -> [a] -> b
foldl1 : (a -> a -> a) -> [a] -> a
foldr1 : (a -> a -> a) -> [a] -> a

scanl  : (a -> b -> b) -> b -> [a] -> [b]
scanl1 : (a -> a -> a) -> [a] -> [a]

filter  : (a -> Bool) -> [a] -> [a]
length  : [a] -> Int
reverse : [a] -> [a]

all : (a -> Bool) -> [a] -> Bool
any : (a -> Bool) -> [a] -> Bool
and : [Bool] -> Bool
or  : [Bool] -> Bool

concat : [[a]] -> [a]
concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = L.concat . L.map f

sum = L.foldl (+) 0
product = L.foldl (*) 1
maximum = L.foldl1 max
minimum = L.foldl1 min

partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred lst =
    case lst of
      []    -> ([],[])
      x::xs -> let (bs,cs) = partition pred xs in
               if pred x then (x::bs,cs) else (bs,x::cs)

zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
zip : [a] -> [b] -> [(a,b)]

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
