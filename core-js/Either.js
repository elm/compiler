/*! Either
!*/


Elm.Either = function() {
 /*[Definition]*/

 /** data Either a b = Left a | Right b
     Represents any data that can take two different types.

     This can also be used for error handling (`Either String a`) where
     error messages are stored on the left, and the correct values
     ("right" values) are stored on the right.
 **/
 function Left(a1) { return ['Left',a1]; }
 function Right(a1){ return ['Right',a1]; }

 /*[Basics]*/

 /** either : (a -> c) -> (b -> c) -> Either a b -> c
     Apply the first function to a `Left` and the second function to a `Right`.
     This allows the extraction of a value from an `Either`.
 **/
 function either(f){ return function(g){ return function(e){
    switch(e[0]){
     case 'Left':  return f(e[1]);
     case 'Right': return g(e[1]);
    }
 };};}

 /** isLeft : Either a b -> Bool
     True if the value is a `Left`.
 **/
 function isLeft(e)  { return e[0] == 'Left';  }

 /** isRight : Either a b -> Bool
     True if the value is a `Right`.
 **/
 function isRight(e) { return e[0] == 'Right'; }

 /*[With Lists]*/

 function get(es) { return Elm.List.map(function(x){return x[1];})(es); }

 /** lefts : [Either a b] -> [a]
     Keep only the values held in `Left` values.
 **/
 function lefts(es) { return get(Elm.List.filter(isLeft)(es)); }

 /** rights : [Either a b] -> [a]
     Keep only the values held in `Right` values.
 **/
 function rights(es) { return get(Elm.List.filter(isRight)(es)); }

 /** partition : [Either a b] -> ([a],[b])
     Split into two lists, lefts on the left and rights on the right.
     So we have the equivalence:

         partition es == (lefts es, rights es)
 **/
 function partition(es) {
     var lrs = Elm.List.partition(isLeft)(es);
     lrs[1] = get(lrs[1]);
     lrs[2] = get(lrs[2]);
     return lrs;
 }
 return {Left:Left,
	 Right:Right,
	 either:either,
	 isLeft:isLeft,
	 isRight:isRight,
	 lefts:lefts,
	 rights:rights,
	 partition:partition};
}();
