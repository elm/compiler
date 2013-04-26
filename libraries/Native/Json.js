
Elm.Native.Json = function(elm) {
  'use strict';

  var Maybe = Elm.Maybe(elm);
  var Dict = Elm.Dict(elm);
  var List = Elm.List(elm);
  var JS = Elm.JavaScript(elm);
  var Utils = Elm.Native.Utils(elm);

  function fromValue(v) {
    switch (v.ctor) {
    case 'Null'   : return null;
    case 'String' : return JS.fromString(v._0);
    case 'Object' :
      var obj = {};
      var array = JS.fromList(Dict.toList(v._0));
      for (var i = arr.length; i--; ) {
	obj[JS.fromString(array[i]._0)] = fromValue(array[i]._1);
      }
      return obj;
    case 'Array'  :
      var array = JS.fromList(v._0);
      for (var i = array.length; i--; ) {
	array[i] = fromValue(array[i]);
      }
      return array;
    default :
      return v._0;
    }
  }

  function toPrettyJSString(sep, obj) {
    return JSON.stringify(fromValue(obj), null, JS.fromString(sep));
  }

  function toValue(v) {
    switch (typeof v) {
    case 'string' : return { ctor:"String", _0: JS.toString(v) };
    case 'number' : return { ctor:"Number", _0: JS.toFloat(v)  };
    case 'boolean': return { ctor:"Bool"  , _0: JS.toBool(v)   };
    case 'object' :
      if (v === null) return { ctor:"Null" };
      if (v instanceof Array) {
          for (var i = v.length; i--; ) { v[i] = toValue(v[i]); }
	  return { ctor:"Array", _0: JS.toList(v) };
      }
      var array = [];
      for (var k in v) array.push(Utils.Tuple2(JS.toString(k), toValue(v[k])));
      return { ctor:"Object", _0: Dict.fromList(JS.toList(array)) };
    }
  }

  function fromJSString(str) {
    try {
	return Maybe.Just(toValue(JSON.parse(str)));
    } catch (e) {
	return Maybe.Nothing;
    }
  }

  function recordFromJSString(str) {
    try {
	return Maybe.Just(JS.toRecord(JSON.parse(str)));
    } catch (e) {
	return Maybe.Nothing;
    }
  }
  function recordToPrettyJSString(sep, rec) {
    return JSON.stringify(JS.fromRecord(rec), null, JS.fromString(sep));
  }

  return elm.Native.Json = {
      toPrettyJSString : F2(toPrettyJSString),
      fromJSString : fromJSString,
      recordToPrettyJSString : F2(recordToPrettyJSString),
      recordFromJSString : recordFromJSString
  };

};
