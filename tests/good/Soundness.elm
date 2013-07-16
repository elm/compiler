
trickyID x = let y = x in y

quad f = twice (twice f)
twice f x = f (f x)

n = quad (quad trickyID) 4
c = twice trickyID 'a'
