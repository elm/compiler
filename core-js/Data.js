
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
	    catMaybes : List.foldr(consMaybe)(["Nil"]),
	    isJust : function(m) { return m[0] === "Just"; },
	    isNothing : function(m) { return m[0] === "Nothing"; },
	    fromMaybe : fromMaybe,
	    consMaybe : consMaybe,
	    mapMaybe : function(f) { return List.foldr(mapCons(f))(["Nil"]); },
	    maybe : maybe
    };
}();

/*
var String = function() {

  function append(s1) { return function(s2) {
	  return s1.concat(s2);
      };
  }
  function map(f) { return function(s) {
      for (var i = s.length; i--; ) { s[i] = f(s[i]); }
    };
  }

  function intercalate(sep) { return function(ss) {
	  return Value.listToArray(ss).join(sep);
      };
  }
  function intersperse(sep) { return function(s) {
	  return s.split("").join(sep);
      };
  }

  function foldl(f) { return function(b) { return function(s) {
        var acc = b;
        for (var i = 0, len = s.length; i < len; ++i) { acc = f(s[i])(acc); }
	return acc;
      };
    };
  }
  function foldr(f) { return function(b) { return function(s) {
        var acc = b;
        for (var i = s.length; i--; ) { acc = f(s[i])(acc); }
	return acc;
      };
    };
  }

  function concatMap(f) { return function(s) {
      var a = s.split("");
      for (var i = a.length; i--; ) { a[i] = f(a[i]); }
      return a.join("");
    };
  }

  function forall(pred) { return function(s) {
       for (var i = s.length; i--; ) { if (!pred(s[i])) {return false}; }
       return true;
    };
  }
  function exists(pred) { return function(s) {
       for (var i = s.length; i--; ) { if (pred(s[i])) {return true}; }
       return false;
    };
  }

  return {cons : append,
	  snoc : append,
	  head : function(s) { return s[0]; },
	  last : function(s) { return s[s.length-1]; },
	  tail : function(s) { return s.slice(1); },
	  length : function(s) { return s.length; },
	  map : map,
	  intercalate : intercalate,
	  intersperse : intersperse,
	  reverse : function(s) { return s.split("").reverse().join(""); },
	  toLower : function(s) { return s.toLowerCase(); },
	  toUpper : function(s) { return s.toUpperCase(); },
	  foldl : foldl,
	  foldr : foldr,
	  concat : function(ss) { return Value.listToArray(ss).join(""); },
	  concatMap : concatMap,
	  forall : forall,
	  exists : exists,
	  //filter : filter,
	  //take:,
	  //drop:,
	  toText : Value.toText, 
	  properEscape : Value.properEscape
    };
}();
*/
return {String: {toText:Value.toText, properEscape:Value.properEscape},
	Char:Char,
	Maybe:Maybe,
	List:List
	};
}();