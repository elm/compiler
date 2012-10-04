
var Prelude = function() {
    var mod = function(x) { return function(y) {
	    var r = x % y;
	    var m = x==0 ? 0 : (y>0 ? (x>=0 ? r : r+y) : -mod(-x)(-y));
	    return m == y ? 0 : m;
	}; };

    var min = function(x) { return function(y) { return Math.min(x,y); }; };
    var max = function(x) { return function(y) { return Math.max(x,y); }; };
    
    var flip=function(f){return function(x){return function(y){return f(y)(x);};};};
    var clamp = function(lo) { return function(hi) {
	    return function(x) { return Math.min(hi, Math.max(lo, x)); }; 
	};
    };
    var curry = function(f) { return function(x) { return function(y) {
		return f(["Tuple2",x,y]); }; };
    };
    var uncurry = function(f) { return function(p) {
	    if (p[0] !== "Tuple2") {
		throw "Function was uncurry'd but was not given a pair.";
	    }
	    return f(p[1])(p[2]); };
    };

    var logBase=function(b){return function(x){return Math.log(x)/Math.log(b);};};
    
    return {id   : function(x) { return x; },
	    not  : function(b) { return !b; },
	    fst  : function(p) { return p[1]; },
	    snd  : function(p) { return p[2]; },
	    rem  : function(x) { return function(y) { return x % y; }; },
	    div  : function(x) { return function(y) { return ~~(x / y); }; },
	    compare : function(x) { return function (y) {
		x = (typeof x === "object") ? toText(x) : x;
		y = (typeof y === "object") ? toText(y) : y;
		return [ x === y ? 'EQ' : (x < y ? 'LT' : 'GT') ];
	      };
	    },
	    toFloat : function(x) { return x; },
	    round : function(n) { return Math.round(n); },
	    floor : function(n) { return Math.floor(n); },
	    ceiling : function(n) { return Math.ceil(n); },
	    truncate : function(n) { return ~~n; },
	    sqrt : Math.sqrt,
	    abs  : Math.abs,
	    pi   : Math.PI,
	    e    : Math.E,
	    sin  : Math.sin,
	    cos  : Math.cos,
	    tan  : Math.tan,
	    asin : Math.asin,
	    acos : Math.acos,
	    atan : Math.atan,
	    atan2 : function(y) { return function(x) { return Math.atan2(y,x); }; },
	    mod  : mod,
	    min  : min,
	    max  : max,
	    flip : flip,
	    clamp : clamp,
	    curry : curry,
	    uncurry : uncurry,
	    logBase : logBase,
	    Just    : Data.Maybe.Just,
	    Nothing : Data.Maybe.Nothing,
	    maybe   : Data.Maybe.maybe,
	    map     : Data.List.map,
	    filter  : Data.List.filter,
	    head    : Data.List.head,
	    tail    : Data.List.tail,
	    last    : Data.List.last,
	    length  : Data.List.length,
	    reverse : Data.List.reverse,
	    foldr   : Data.List.foldr,
	    foldr1  : Data.List.foldr1,
	    foldl   : Data.List.foldl,
	    foldl1  : Data.List.foldl1,
	    and     : Data.List.and,
	    or      : Data.List.or,
	    all     : Data.List.all,
	    any     : Data.List.any,
	    sum     : Data.List.sum,
	    product : Data.List.product,
	    concat  : Data.List.concat,
	    concatMap : Data.List.concatMap,
	    maximum : Data.List.maximum,
	    minimum : Data.List.minimum,
	    scanl   : Data.List.scanl,
	    scanl1  : Data.List.scanl1,
	    take    : Data.List.take,
	    drop    : Data.List.drop,
	    lift  : Signal.lift,
	    lift2 : Signal.lift2,
	    lift3 : Signal.lift3,
	    lift4 : Signal.lift4,
	    foldp : Signal.foldp,
	    foldp1 : Signal.foldp1,
	    foldp_ : Signal.foldp_,
	    constant : Signal.constant,
	    count : Signal.count,
	    keepIf : Signal.keepIf,
	    dropIf : Signal.dropIf,
	    keepWhen : Signal.keepWhen,
	    dropWhen : Signal.dropWhen,
	    dropRepeats : Signal.dropRepeats,
	    sampleOn : Signal.sampleOn
	    };
}();