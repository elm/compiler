
apply : (a -> b) -> a -> b
apply f = let g x = f x
          in  g
