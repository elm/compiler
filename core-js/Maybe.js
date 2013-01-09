
Elm.Maybe = function() {
    function consMaybe(x) { return function(xs) {
	    if (x[0] === "Just") return ["Cons", x[1], xs];
	    return xs;
	};
    }
    function mapCons(f) { return function(y) { return function(xs) {
		var x = f(y);
		if (x[0] === "Just") return ["Cons", x[1], xs];
		return xs;
	    };
	};
    }
    function maybe(b) { return function(f) { return function(m) {
		if (m[0] === "Just") return f(m[1]);
		return b;
	    };
	};
    }

    return {Just : function(x) { return ["Just",x]; },
	    Nothing : ["Nothing"],
	    justs : Elm.List.foldr(consMaybe)(["Nil"]),
	    isJust : function(m) { return m[0] === "Just"; },
	    isNothing : function(m) { return m[0] === "Nothing"; },
	    cons : consMaybe,
	    maybe : maybe
    };
}();
