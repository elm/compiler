
var Data = function() {

var Char = function() {
    return {fromCode : function(c) { return String.fromCharCode(c); },
	    toCode : function(c) { return c.charCodeAt(0); },
	    toUpper : function(c) { return c.toUpperCase(); },
	    toLower : function(c) { return c.toLowerCase(); },
	    toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
	    toLocaleLower : function(c) { return c.toLocaleLowerCase(); }
    };
}();

var Maybe = function() {
    function consMaybe(x) { return function(xs) {
	    if (x[0] === "Just") return ["Cons", x[1], xs];
	    return xs;
	};
    }
    function fromMaybe(b) { return function(m) {
	    if (m[0] === "Just") return m[1];
	    return b;
	};
    }
    function mapCons(f) { return function(y) { return function(xs) {
		var x = f(x);
		if (x[0] === "Just") return ["Cons", x[1], xs];
		return xs;
	    };
	};
    };

    return {catMaybes : List.foldr(consMaybe)(["Nil"]),
	    isJust : function(m) { return m[0] === "Just"; },
	    isNothing : function(m) { return m[0] === "Nothing"; },
	    fromMaybe : fromMaybe,
	    consMaybe : consMaybe,
	    mapMaybe : function(f) { return List.foldr(mapCons(f))(["Nil"]); }
    };
}();

return {String: { toText : Value.toText , properEscape : Value.properEscape } ,
	Char:Char,
	Maybe:Maybe,
	List:List
	};
}();