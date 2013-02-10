
module List where

import Native.List as Native

head : [a] -> a
head = Native.head

tail : [a] -> [a]
tail = Native.tail

map : (a -> b) -> [a] -> [b]
map = Native.map

foldl : (a -> b -> b) -> b -> [a] -> b
foldl = Native.foldl

foldr : (a -> b -> b) -> b -> [a] -> b
foldr = Native.foldr

foldl1 : (a -> a -> a) -> [a] -> a
foldl1 f (x::xs) = foldl f x xs

foldr1 : (a -> a -> a) -> [a] -> a
foldr1 f (x::xs) = foldr f x xs

scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl f b xs = b :: (case xs of { x::xs -> scanl f (f x b) xs; [] -> [] })

scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 f lst = case lst of { x::xs -> scanl f x xs; [] -> [] }

filter : (a -> Bool) -> [a] -> [a]
filter = Native.filter

length : [a] -> Int
length = Native.length

reverse : [a] -> [a]
reverse = foldl (::) []

concat = foldr (++) []
concatMap f = concat . map f

and : [Bool] -> Bool
and = foldl (&&) True

or : [Bool] -> Bool
or = foldl (||) False

any : (a -> Bool) -> [a] -> Bool
any pred = foldl (\x acc -> acc && pred x) True

all : (a -> Bool) -> [a] -> Bool
all pred = foldl (\x acc -> acc || pred x) False

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
