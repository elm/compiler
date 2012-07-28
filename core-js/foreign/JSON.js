
var ElmJSON = function() {
    var JS = Foreign.JavaScript;
    var empty = ['JSON',{}];
    function singleton(k) { return function(v) {
	    var obj = {};
	    obj[JS.castStringToJSString(k)] = v;
	    return ['JSON', obj ];
	};
    }
    function insert(k) { return function(v) { return function(inObj) {
          var obj = inObj[1];
	  var outObj = {};
          for (var i in obj) {
	      outObj[i] = obj[i];
	  }
	  outObj[JS.castStringToJSString(k)] = v;
	  return ['JSON', outObj ];
        };
      };
    }
    function lookup(key) { return function(obj) {
        var k = JS.castStringToJSString(key);
        return obj[1].hasOwnProperty(k) ? Just(obj[1][k]) : Nothing ;
      };
    }
    function lookupWithDefault(base) { return function(key) { return function(obj) {
          var k = JS.castStringToJSString(key);
          return obj[1].hasOwnProperty(k) ? obj[1][k] : base ;
        };
      };
    }
    function remove(k) { return function(inObj) {
        var obj = inObj[1];
        var outObj = {};
	for (var i in obj) {
	    outObj[i] = obj[i];
	}
	delete outObj[JS.castStringToJSString(k)];
	return ['JSON', outObj];
      };
    }

    function JSString(v) { return [ "JSString", v ]; }
    function JSNumber(v) { return [ "JSNumber", v ]; }
    function JSBool(v) { return [ "JSBool", v ]; }
    var JSNull = [ "JSNull" ];
    function JSArray(v) { return [ "JSArray", v ]; }
    function JSObject(v) { return [ "JSObject", v ]; }

    function toList(json) {
	var obj = json[1];
	var arr = [];
	for (var i in obj) {
	    arr.push(Value.Tuple(JS.castJSStringToString(i), obj[i]));
	}
	return JS.castJSArrayToList(arr);
    }
    function fromList(list) {
	var arr = JS.castListToJSArray(list);
	var obj = {};
	for (var i = arr.length; i--; ) {
	    obj[JS.castStringToJSString(arr[i][1])] = arr[i][2];
	}
	return [ "JSON", obj ];
    }

    function toPrettyJSString(sep) { return function (obj) {
	function fromValue(v) {
	    switch (v[0]) {
	    case 'JSNull'   : return null;
	    case 'JSString' : return JS.castStringToJSString(v[1]);
	    case 'JSObject' :
	    var o = {};
	    var from = v[1][1];
	    for (var i in from) {
		o[i] = fromValue(from[i]);
	    }
	    return o;
	    case 'JSArray'  :
	    var a = JS.castListToJSArray(v[1]);
	    for (var i = a.length; i--; ) {
		a[i] = fromValue(a[i]);
	    }
	    return a;
	    default :
	    return v[1];
	    }
	}
	return JSON.stringify(fromValue([ 'JSObject', obj ]), null, JS.castStringToJSString(sep));
      };
    }
    function fromJSString(str) {
	var obj = JSON.parse(str);
	function toValue(v) {
	    switch (typeof v) {
	    case 'string'  : return [ "JSString", JS.castJSStringToString(v) ];
	    case 'number'  : return [ "JSNumber", JS.castJSNumberToFloat(v) ];
	    case 'boolean' : return [ "JSBool", JS.castJSBoolToBool(v) ];
	    case 'object'  :
		if (v === null) return [ "JSNull" ];
		for (var i in v) {
		    v[i] = toValue(v[i]);
		}
		if (v instanceof Array) return [ "JSArray", JS.castJSArrayToList(v) ];
		return [ "JSObject",  [ "JSON", v ] ];
	    }
	}
	for (var i in obj) {
	    obj[i] = toValue(obj[i]);
	}
	return ['JSON',obj];
    }
    return {empty : empty,
	    singleton : singleton,
	    insert : insert,
	    lookup : lookup,
	    lookupWithDefault : lookupWithDefault,
	    remove : remove,
	    toPrettyJSString : toPrettyJSString,
	    toJSString : toPrettyJSString(''),
	    fromJSString : fromJSString,
	    toPrettyString : function(sep) { return function(v) {
		return JS.castJSStringToString(toPrettyJSString(sep)(v)); }; },
	    toString : function(v) { return JS.castJSStringToString(toPrettyJSString('')(v)); },
	    fromString : function(v) { return fromJSString(JS.castStringToJSString(v)); },
	    toList : toList,
	    fromList : fromList,
	    JSString : JSString,
	    JSNumber : JSNumber,
	    JSBool : JSBool,
	    JSNull : JSNull,
	    JSArray : JSArray,
	    JSObject : JSObject
    };
}();

Foreign.JavaScript.JSON = ElmJSON;