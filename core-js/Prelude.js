
var Elm = Elm || {};
Elm.Prelude = function() {
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
    
    return {eq   : Value.eq,
	    id   : function(x) { return x; },
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
	    Just    : Maybe.Just,
	    Nothing : Maybe.Nothing,
	    maybe   : Maybe.maybe,
	    map     : List.map,
	    filter  : List.filter,
	    head    : List.head,
	    tail    : List.tail,
	    last    : List.last,
	    length  : List.length,
	    reverse : List.reverse,
	    foldr   : List.foldr,
	    foldr1  : List.foldr1,
	    foldl   : List.foldl,
	    foldl1  : List.foldl1,
	    and     : List.and,
	    or      : List.or,
	    all     : List.all,
	    any     : List.any,
	    sum     : List.sum,
	    product : List.product,
	    concat  : List.concat,
	    concatMap : List.concatMap,
	    maximum : List.maximum,
	    minimum : List.minimum,
	    scanl   : List.scanl,
	    scanl1  : List.scanl1,
	    take    : List.take,
	    drop    : List.drop,
	    zip     : List.zip,
	    unzip   : List.unzip,
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