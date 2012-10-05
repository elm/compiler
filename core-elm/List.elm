
head (h:t) = h
tail (h:t) = t

map f lst = case lst of { x:xs -> f x : map f xs; [] -> [] }
foldl f b lst = case lst of { x:xs -> foldl f (f x b) xs; [] -> b }
foldr f b lst = case lst of { x:xs -> f x (foldr f b xs); [] -> b }

foldl1 f (x:xs) = foldl f x xs
foldr1 f (x:xs) = foldr f x xs

scanl f b xs = b : (case xs of { x:xs -> scanl f (f x b) xs; [] -> [] })
scanl1 f lst = case lst of { x:xs -> scanl f x xs; [] -> [] }

filter pred lst =
    case lst of
    { x:xs -> if pred x then x : filter pred xs else filter pred xs
    ; [] -> [] }

length = foldl (\x c -> 1 + c) 0
reverse = foldl (:) []

concat = foldr (++) []
concatMap f = concat . map f

and = foldl (&&) True
or = foldl (||) False

forall pred = foldl (\x acc -> acc && pred x) True
exists pred = foldl (\x acc -> acc || pred x) False

sum = foldl (+) 0
product = foldl (*) 1
maximum = foldl1 max
minimum = foldl1 min

partition pred lst =
    case lst of
    { x:xs -> case partition pred xs of
              { (as,bs) -> if pred x then (x:as,bs) else (as,x:bs) }
    ; [] -> ([],[]) }

zipWith f listA listB = case (listA, listB) of
    { (a:as, b:bs) -> f a b : zipWith f as bs; _ -> [] }

zip listA listB = case (listA, listB) of
    { (a:as, b:bs) -> (a,b) : zip as bs; _ -> [] }

unzip pairs =
  case pairs of
  { p:ps -> case (p, unzip ps) of { ((x,y),(xs,ys)) -> (x:xs,y:ys) }
  ; [] -> ([],[]) }

intersperse sep xs =
  case xs of { a:b:cs -> a : sep : intersperse sep (b:cs)
             ; a:[] -> [a] ; [] -> [] }
intercalate sep xs =
  case xs of { a:b:cs -> a ++ sep ++ intercalate sep (b:cs)
             ; a:[] -> a ; [] -> [] }
