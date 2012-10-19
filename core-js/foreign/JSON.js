
var Elm = Elm || {};
var JSjson = JSON;
Elm.JSON = function() {
    var JS = Elm.JavaScript;
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

    function JsonString(v) { return [ "JsonString", v ]; }
    function JsonNumber(v) { return [ "JsonNumber", v ]; }
    function JsonBool(v) { return [ "JsonBool", v ]; }
    var JsonNull = [ "JsonNull" ];
    function JsonArray(v) { return [ "JsonArray", v ]; }
    function JsonObject(v) { return [ "JsonObject", v ]; }

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
	    case 'JsonNull'   : return null;
	    case 'JsonString' : return JS.castStringToJSString(v[1]);
	    case 'JsonObject' :
	    var o = {};
	    var from = v[1][1];
	    for (var i in from) {
		o[i] = fromValue(from[i]);
	    }
	    return o;
	    case 'JsonArray'  :
	    var a = JS.castListToJSArray(v[1]);
	    for (var i = a.length; i--; ) {
		a[i] = fromValue(a[i]);
	    }
	    return a;
	    default :
	    return v[1];
	    }
	}
	return JSjson.stringify(fromValue([ 'JsonObject', obj ]), null, JS.castStringToJSString(sep));
      };
    }
    function fromJSString(str) {
	var obj = JSjson.parse(str);
	function toValue(v) {
	    switch (typeof v) {
	    case 'string'  : return [ "JsonString", JS.castJSStringToString(v) ];
	    case 'number'  : return [ "JsonNumber", JS.castJSNumberToFloat(v) ];
	    case 'boolean' : return [ "JsonBool", JS.castJSBoolToBool(v) ];
	    case 'object'  :
		if (v === null) return [ "JsonNull" ];
		for (var i in v) {
		    v[i] = toValue(v[i]);
		}
		if (v instanceof Array) return [ "JsonArray", JS.castJSArrayToList(v) ];
		return [ "JsonObject",  [ "JSON", v ] ];
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
	    findWithDefault : lookupWithDefault,
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
	    JsonString : JsonString,
	    JsonNumber : JsonNumber,
	    JsonBool : JsonBool,
	    JsonNull : JsonNull,
	    JsonArray : JsonArray,
	    JsonObject : JsonObject
    };
}();
