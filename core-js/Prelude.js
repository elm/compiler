
Value.addListener(document, 'elm_log', function(e) { console.log(e.value); });
Value.addListener(document, 'elm_title', function(e) {document.title = e.value;});
Value.addListener(document, 'elm_redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
    });

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

    function readInt(str) {
	var s = Elm.JavaScript.castStringToJSString(str);
	var len = s.length;
	if (len === 0) { return ["Nothing"]; }
	var start = 0;
	if (s[0] == '-') {
	    if (len === 1) { return ["Nothing"]; }
	    start = 1;
	}
	for (var i = start; i < len; ++i) {
	    if (!Elm.Char.isDigit(s[i])) { return ["Nothing"]; }
	}
	return ["Just", parseInt(s)];
    }

    function readFloat(str) {
	var s = Elm.JavaScript.castStringToJSString(str);
	var len = s.length;
	if (len === 0) { return ["Nothing"]; }
	var start = 0;
	if (s[0] == '-') {
	    if (len === 1) { return ["Nothing"]; }
	    start = 1;
	}
	var dotCount = 0;
	for (var i = start; i < len; ++i) {
	    if (Elm.Char.isDigit(s[i])) { continue; }
	    if (s[i] === '.') {
		dotCount += 1;
		if (dotCount <= 1) { continue; }
	    }
	    return ["Nothing"];
	}
	return ["Just", parseFloat(s)];
    }

    function compare(x) { return function (y) {
      if (x instanceof Array && y instanceof Array) {
	var len = x.length;
	if (len == y.length) {
	  for (var i = 1; i < len; ++i) {
	    var cmp = compare(x[i])(y[i]);
	    if (cmp[0] === 'EQ') continue;
	    return cmp;
	  }
	  return ['EQ'];
	}
	return [ y.length == 1 ? 'GT' : 'LT' ];
      }
      return [ x === y ? 'EQ' : (x < y ? 'LT' : 'GT') ];
     };
    }
    return {eq   : Value.eq,
	    id   : function(x) { return x; },
	    not  : function(b) { return !b; },
	    fst  : function(p) { return p[1]; },
	    snd  : function(p) { return p[2]; },
	    rem  : function(x) { return function(y) { return x % y; }; },
	    div  : function(x) { return function(y) { return ~~(x / y); }; },
	    otherwise : true,
	    compare : compare,
	    toFloat : function(x) { return x; },
	    round : function(n) { return Math.round(n); },
	    floor : function(n) { return Math.floor(n); },
	    ceiling : function(n) { return Math.ceil(n); },
	    truncate : function(n) { return ~~n; },
	    readInt : readInt,
	    readFloat : readFloat,
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
	    Just    : Elm.Maybe.Just,
	    Nothing : Elm.Maybe.Nothing,
	    maybe   : Elm.Maybe.maybe,
	    map     : Elm.List.map,
	    zip     : Elm.List.zip,
	    zipWith : Elm.List.zipWith,
	    filter  : Elm.List.filter,
	    head    : Elm.List.head,
	    tail    : Elm.List.tail,
	    last    : Elm.List.last,
	    length  : Elm.List.length,
	    reverse : Elm.List.reverse,
	    foldr   : Elm.List.foldr,
	    foldr1  : Elm.List.foldr1,
	    foldl   : Elm.List.foldl,
	    foldl1  : Elm.List.foldl1,
	    and     : Elm.List.and,
	    or      : Elm.List.or,
	    all     : Elm.List.all,
	    any     : Elm.List.any,
	    sum     : Elm.List.sum,
	    product : Elm.List.product,
	    concat  : Elm.List.concat,
	    concatMap : Elm.List.concatMap,
	    maximum : Elm.List.maximum,
	    minimum : Elm.List.minimum,
	    scanl   : Elm.List.scanl,
	    scanl1  : Elm.List.scanl1,
	    take    : Elm.List.take,
	    drop    : Elm.List.drop,
	    zip     : Elm.List.zip,
	    unzip   : Elm.List.unzip,
	    lift  : Elm.Signal.lift,
	    lift2 : Elm.Signal.lift2,
	    lift3 : Elm.Signal.lift3,
	    lift4 : Elm.Signal.lift4,
	    lift5 : Elm.Signal.lift5,
	    lift6 : Elm.Signal.lift6,
	    lift7 : Elm.Signal.lift7,
	    lift8 : Elm.Signal.lift8,
	    foldp : Elm.Signal.foldp,
	    foldp1 : Elm.Signal.foldp1,
	    foldp_ : Elm.Signal.foldp_,
	    constant : Elm.Signal.constant,
	    merge : Elm.Signal.merge,
	    count : Elm.Signal.count,
	    countIf : Elm.Signal.countIf,
	    average : Elm.Signal.average,
	    keepIf : Elm.Signal.keepIf,
	    dropIf : Elm.Signal.dropIf,
	    keepWhen : Elm.Signal.keepWhen,
	    dropWhen : Elm.Signal.dropWhen,
	    dropRepeats : Elm.Signal.dropRepeats,
	    sampleOn : Elm.Signal.sampleOn,
	    timestamp : Elm.Signal.timestamp,
	    timeOf : Elm.Signal.timeOf
	    };

}();

(function() {
  var include = function(library) {
    for (var i in library) {
	Elm.Prelude[i] = library[i];
    }
  };
  include (Elm.Color);
  include (Elm.Text);
  include (Elm.Graphics);
  include (Elm.Time);

  show = Value.show;
  
}());
