test : (a -> b) -> a -> b
test f = let g x = f x
         in  g
