/**
module Native.JavaScript where
**/

(function() {
  'use strict';

  var fromArray = Elm.Native.List.fromArray;
  var toArray = Elm.Native.List.toArray;

  function fromJS(v) {
    var type = typeof v;
    if (type === 'number' ) return v;
    if (type === 'boolean') return v;
    if (type === 'string' ) return fromArray(v);
    if (v instanceof Array) {
      var arr = [];
      var len = v.length;
      for (var i = 0; i < len; ++i) {
	var x = fromJS(v[i]);
	if (x !== null) arr.push(x);
      }
      return fromArray(arr);
    }
    if (type === 'object') {
      var rec = { _:{} };
      for (var f in v) {
	var x = fromJS(v[f]);
	if (x !== null) rec[f] = x;
      }
      return rec;
    }
    return null;
  }

  function toJS(v) {
    var type = typeof v;
    if (type === 'number' || type === 'boolean') return v;
    if (type === 'object' && '_' in v) {
	var obj = {};
	for (var k in v) {
	    var x = v[k];
	    if (x !== null) obj[k] = x;
	}
	return obj;
    }
    if (type === 'object' && (v.ctor === 'Cons' || v.ctor === 'Nil')) {
	var array = toArray(v);
	for (var i = array.length; i--; ) {
	    array[i] = toJS(array[i]);
	}
	if (typeof array[0] === 'string') array.join('');
	return array;
    }
    return null;
  }
  
  function fromRecord(r) {
    if (typeof r === 'object' && '_' in r) {
      return toJS(r);
    }
    throw new Error("'fromRecord' must be called on a record.");
  }

  Elm.Native.JavaScript = {
      id         : function(n) { return n },
      toInt      : function(n) { return n|0 },
      toString   : function(s) { return fromArray(s) },
      toList     : fromArray,
      fromString : function(s) { return toArray(s).join('') },
      fromList   : toArray,
      toRecord   : fromJS,
      fromRecord : fromRecord
  };

}());