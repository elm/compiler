-- unify flexible and locked types
import Dict

multiplyKey ((i,j), _) = (i*j)

x = map multiplyKey (Dict.toList (Dict.singleton (1,2) 0))
