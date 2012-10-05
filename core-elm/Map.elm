
empty = []
singleton k v = [(k,v)]
insert k v dict = (k,v):dict

fst pair = let (a,b) = pair in a

member key = exists (\pair -> fst pair == key)

delete key = filter (\pair -> fst pair == key)
intersect d1 d2 = filter (\pair -> member (fst pair) d2) d1
difference d1 d2 = filter (\pair -> not (member (fst pair) d2)) d1
union d1 d2 = d1 ++ difference d2 d1