let apply f x = f x
    id x = x
 in apply id (apply id (apply id (apply id (apply id (apply id 4)))))
