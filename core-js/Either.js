
Elm.Either = function() {
 function Left(a1) { return ['Left',a1]; }
 function Right(a1){ return ['Right',a1]; }
 function either(f){ return function(g){ return function(e){
    switch(e[0]){
     case 'Left':  return f(e[1]);
     case 'Right': return g(e[1]);
    }
 };};}
 function isLeft(e)  { return e[0] == 'Left';  }
 function isRight(e) { return e[0] == 'Right'; }

 function get(es) { return Elm.List.map(function(x){return x[1];})(es); }
 function lefts(es) { return get(Elm.List.filter(isLeft)(es)); }
 function rights(es) { return get(Elm.List.filter(isRight)(es)); }
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
