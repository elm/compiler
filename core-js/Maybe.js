/*! Maybe
!*/

/*[Definition]*/

/** data Maybe a = Just a | Nothing
    The Maybe datatype. Useful when a computation may or may not
    result in a value (e.g. logarithm is defined only for positive numbers).
**/

/*[Basic Utilities]*/

/** maybe : b -> (a -> b) -> Maybe a -> b
    Apply a function to the contents of a `Maybe`.
    Return default when given `Nothing`.
**/
/** isJust : Maybe a -> Bool
    Check if constructed with `Just`.
**/
/** isNothing : Maybe a -> Bool
    Check if constructed with `Nothing`.
**/

/*[Maybe with Lists]*/

/** cons : Maybe a -> [a] -> [a]
    If `Just`, adds the value to the front of the list.
    If `Nothing`, list is unchanged.
**/
/** justs : [Maybe a] -> [a]
    Filters out Nothings and extracts the remaining values.
**/

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
