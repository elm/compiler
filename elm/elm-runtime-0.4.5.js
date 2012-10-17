
var Guid = function() {
 var counter = 0;
 var guid = function() { counter += 1; return counter; };
 return {guid : guid};
}();
var Elm = Elm || {};
Elm.JavaScript = function() {
  function castJSBoolToBool(b) { return b; }
  function castBoolToJSBool(b) { return b; }

  function castJSNumberToFloat(n) { return n; }
  function castFloatToJSNumber(n) { return n; }
  
  function castJSNumberToInt(n) { return ~~n; }
  function castIntToJSNumber(n) { return n; }

  function castJSElementToElement(w) {
    return function(h) {
      return function(node) {
	return ["Element",Guid.guid(),
		["EExternalHtml",node],
		w,h,1,Nothing,Nothing];
      }
    }
  }
  function castElementToJSElement(elem) { return Render.render(elem); }

  function castJSArrayToList(arr) {
      var list = ["Nil"];
      for (var i = arr.length; i--; ) {
	  list = [ "Cons", arr[i], list ];
      }
      return list;
  }
  function castListToJSArray(list) {
      var a = [];
      while (list[0] === "Cons") {
	  a.push(list[1]);
	  list = list[2];
      }
      return a;
  }
  
  var castJSStringToString = castJSArrayToList;
  function castStringToJSString(str) {
      if (typeof str === "string") return str;
      return castListToJSArray(str).join('');
  }
  
  function fromTuple(t) { return t.slice(1); }
  function toTuple(a) { return ["Tuple" + a.length].concat(a); }
  
  return {castJSBoolToBool:castJSBoolToBool,
	  castBoolToJSBool:castBoolToJSBool,
	  castJSNumberToFloat:castJSNumberToFloat,
	  castFloatToJSNumber:castFloatToJSNumber,
	  castJSNumberToInt:castJSNumberToInt,
	  castIntToJSNumber:castIntToJSNumber,
	  Experimental : {castJSElementToElement:castJSElementToElement,
			  castElementToJSElement:castElementToJSElement},
	  castJSArrayToList:castJSArrayToList,
	  castListToJSArray:castListToJSArray,
	  castJSStringToString:castJSStringToString,
	  castStringToJSString:castStringToJSString,
	  castTupleToJSTuple2:fromTuple,
	  castTupleToJSTuple3:fromTuple,
	  castTupleToJSTuple4:fromTuple,
	  castTupleToJSTuple5:fromTuple,
	  castJSTupleToTuple2:toTuple,
	  castJSTupleToTuple3:toTuple,
	  castJSTupleToTuple4:toTuple,
	  castJSTupleToTuple5:toTuple
  };
}();

var Elm = Elm || {};
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
	return JSON.stringify(fromValue([ 'JsonObject', obj ]), null, JS.castStringToJSString(sep));
      };
    }
    function fromJSString(str) {
	var obj = JSON.parse(str);
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
var Value = function(){

  var eq = function(x,y) {
    if (typeof x === "object") {
	if (x === y) return true;
	if (x.length !== y.length) return false;
	for (var i = x.length; i--; ) {
	    if (!eq(x[i],y[i])) return false;
	}
	return true;
    }
    return x === y;
  };

  var Tuple = function() {
      var len = arguments.length;
      var arr = new Array(len+1);
      arr[0] = "Tuple" + arguments.length;
      for (var i = len; i--; ) {
	  arr[i+1] = arguments[i];
      }
      return arr;
  };

  var listToArray = function(list) {
      var arr = [];
      while (list[0] === "Cons") {
        arr.push(list[1]);
	list = list[2];
      }
      return arr;
  };

  function makeSpaces(s) {
    if (s.length == 0) { return s; }
    var arr = s.split('');
    if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
    for (var i = arr.length; --i; ) {
      if (arr[i][0] == ' ' && arr[i-1] == ' ') {
        arr[i-1] = arr[i-1] + arr[i];
        arr[i] = '';
      }
    }
    for (var i = arr.length; i--; ) {
      if (arr[i].length > 1 && arr[i][0] == ' ') {
        var spaces = arr[i].split('');
        for (var j = spaces.length - 2; j >= 0; j -= 2) {
          spaces[j] = '&nbsp;';
        }
        arr[i] = spaces.join('');
      }
    }
    return arr.join('');
  }

  function properEscape(str) {
    if (str.length == 0) return str;
    str = str //.replace(/&/g,  "&#38;")
             .replace(/"/g, /*"*/  "&#34;")
             .replace(/'/g, /*'*/  "&#39;")
             .replace(/</g,  "&#60;")
             .replace(/>/g,  "&#62;")
             .replace(/\n/g, "<br/>");
    var arr = str.split('<br/>');
    for (var i = arr.length; i--; ) {
	arr[i] = makeSpaces(arr[i]);
    }
    return arr.join('<br/>');
  }

  var toText = function(elmList) {
    if (typeof elmList === "string") return properEscape(elmList);
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return properEscape(a.join(''));
  };

  function getTextSize(w,h,txt) {
    var t = document.createElement('div');
    t.innerHTML = txt;
    t.style.textAlign = 'left';
    if (w > 0) { t.style.width  = w + "px"; }
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";
    
    document.body.appendChild(t);
    var cStyle = window.getComputedStyle(t);
    var realW = cStyle.getPropertyValue("width").slice(0,-2) - 0;
    var realH = cStyle.getPropertyValue("height").slice(0,-2) - 0;
    document.body.removeChild(t);
    delete t;
    return [Math.ceil(realW),Math.ceil(Math.max(h,realH))];
  }

  function getSize(e) {
    var t = e.cloneNode(true);
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";
    
    document.body.appendChild(t);
    var w = t.offsetWidth;
    var h = t.offsetHeight;
    document.body.removeChild(t);
    delete t;
    return [w,h];
  }

  function getExcess(e) {
    var t = e.cloneNode(true);
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";
    
    document.body.appendChild(t);
    var ow = t.offsetWidth;
    var oh = t.offsetHeight;
    var cStyle = window.getComputedStyle(t);
    var w = cStyle.getPropertyValue("width").slice(0,-2) - 0;
    var h = cStyle.getPropertyValue("height").slice(0,-2) - 0;
    document.body.removeChild(t);
    delete t;
    return [ow-w,oh-h];
  }


  function groupForms(forms) {
    forms = Elm.JavaScript.castListToJSArray(forms);
    var groups = [];
    var arr = [];
    for (var i = forms.length; i--; ) {
	var f = forms[i];
	switch(f[4][0]) {
	case "FElement":
	    if (arr.length > 0) {
		groups.push(arr);
		arr = [];
	    }
	    groups.push(f);
	    break;
	default:
	    arr.push(f);
	}
    }
    if (arr.length > 0) groups.push(arr);
    return groups;
  }

  var toString = function(v) {
    if (typeof v === "boolean") {
	return v ? "True" : "False";
    } else if (typeof v === "number") {
	return v+"";
    } else if (typeof v === "string" && v.length < 2) {
	return "'"+v+"'";
    } else if (v[0]) {
	if (v[0].substring(0,5) === "Tuple") {
	    var output = "";
	    for (var i = v.length; --i; ) {
		output = "," + toString(v[i]) + output;
	    }
	    if (output[0] === ",") output = output.substring(1);
	    return "("+output+")";
	} else if (v[0] === "Cons") {
	    var start = (typeof v[1] === "string") ? '"' : "[";
	    var  end  = (typeof v[1] === "string") ? '"' : "]";
	    var  div  = (typeof v[1] === "string") ?  "" : ",";
	    var   f   = (typeof v[1] === "string") ? function(x){return x} : toString;
	    var output = start + f(v[1]);
	    v = v[2];
	    while (true) {
		if (v[0] === "Cons") {
		    output += div + f(v[1]);
		    v = v[2];
		} else {
		    return output + end;
		}
	    }
	} else if (v[0] === "Nil") {
	    return "[]";
	} else if (v[0] === "JSON") {
	    return "(JSON.fromList " + toString(ElmJSON.toList(v)) + ")";
	} else if (v[0] === "RBNode" || v[0] === "RBEmpty") {
	    function cons(k){ return function(v) { return function(acc) { return ["Cons",["Tuple2",k,v],acc]; }; }; }
	    return "(Map.fromList " + toString(Elm.Dict.fold(cons)(["Nil"])(v)) + ")";
	} else {
	    var output = "";
	    for (var i = v.length; --i; ) {
		output = " " + toString(v[i]) + output
	    }
	    output = v[0] + output;
	    return (v.length > 1) ? "(" + output + ")" : output;
	}
    }
    return v+"";
  };
  var show = function(v) { return str(toString(v)); };
  var append = function(xs,ys) {
    if (typeof xs === "string" && typeof ys === "string") {
	return xs.concat(ys);
    }
    if (xs[0] === "Nil") {
	return ys;
    }
    var root = ["Cons", xs[1], ["Nil"]];
    var curr = root;
    xs = xs[2];
    while (xs[0]==="Cons") {
	curr[2] = ["Cons", xs[1], ["Nil"]];
	xs = xs[2];
	curr = curr[2];
    }
    curr[2] = ys;
    return root;
  };

  var str = function(s) {
    var out = ["Nil"];
    for (var i = s.length; i--; ) {
      out = ["Cons", s[i], out];
    }
    return out;
  };
  
  function wrap(elem) {
      var p = Value.getSize(elem);
      return ["Element", Guid.guid(), ["EHtml",elem],
	      p[0], p[1], 1, Nothing, Nothing];
  }
  var addListener = function() {
      if(document.addEventListener) {
	  return function(element, event, handler) {
	      element.addEventListener(event, handler, false);
	  };
      } else {
	  return function(element, event, handler) {
	      element.attachEvent('on' + event, handler);
	  };
      }
  }();

  return {eq:eq,
	  str:str,
	  show:show,
	  Tuple:Tuple,
	  append:append,
	  listToArray:listToArray,
	  toText : toText,
	  properEscape : properEscape,
	  getTextSize : getTextSize,
	  getSize : getSize,
	  getExcess : getExcess,
	  groupForms : groupForms,
	  wrap : wrap,
	  addListener : addListener
       };
}();var Elm = Elm || {};
Elm.List = function() {

    var throwError = function(f) {
	throw "Function '" + f + "' expecting a list!";
    }

    function length(xs) {
	var out = 0;
	while (xs[0] === "Cons") {
	    out += 1;
	    xs = xs[2];
	}
	return out;
    };
    var reverse = foldl(function(x_72) {
      return function(y_73) {
        return["Cons", x_72, y_73]
      }
    })(["Nil"]);
    var concat = foldr(function(x_74) {
      return function(y_75) {
        return Value.append(x_74, y_75)
      }
    })(["Nil"]);
    var and = foldl(function(x_77) {
      return function(y_78) {
        return x_77 && y_78
      }
    })(true);
    var or = foldl(function(x_79) {
      return function(y_80) {
        return x_79 || y_80
      }
    })(false);
    var sum = foldl(function(x_89) {
      return function(y_90) {
        return x_89 + y_90
      }
    })(0);
    var product = foldl(function(x_91) {
      return function(y_92) {
        return x_91 * y_92
      }
    })(1);
    var maximum = foldl1(function(x) { return function(y) { return Math.max(x,y) } });
    var minimum = foldl1(function(x) { return function(y) { return Math.min(x,y) } });
    function head(v) {
	if (v[0] !== "Cons") {
	    throw "Error: 'head' only accepts lists of length greater than one.";
	}
	return v[1];
    }
    function tail(v) {
	if (v[0] !== "Cons") {
	    throw "Error: 'tail' only accepts lists of length greater than one.";
	}
	return v[2];
    }
    function last(v) {
	if (v[0] !== "Cons") {
	    throw "Error: 'last' only accepts lists of length greater than one.";
	}
	var out = v[1];
	while (v[0] === "Cons") {
	    out = v[1];
	    v = v[2];
	}
	return out;
    }
    function map(f) {
      return function(xs) {
	  if (xs[0] === "Nil") { return xs; }
	  if (xs[0] !== "Cons") { throwError('map'); }
	  var root = ["Cons", f(xs[1]), ["Nil"]];
	  var curr = root;
	  xs = xs[2];
	  while (xs[0]==="Cons") {
	      curr[2] = ["Cons", f(xs[1]), ["Nil"]];
	      xs = xs[2];
	      curr = curr[2];
	  }
	  return root;
      }
    }
    function foldl(f) {
      return function(b) {
        return function(xs) {
          var acc = b;
	  if (xs[0] === "Nil") { return acc; }
	  if (xs[0] !== "Cons") { throwError('foldl'); }
	  while (xs[0] === "Cons") {
	      acc = f(xs[1])(acc);
	      xs = xs[2];
	  }
	  return acc;
        }
      }
    }
    function foldr(f) {
      return function(b) {
        return function(xs) {
          var acc = b;
	  if (xs[0] === "Nil") { return acc; }
	  if (xs[0] !== "Cons") { throwError('foldr'); }
	  var arr = [];
	  while (xs[0] === "Cons") {
	      arr.push(xs[1]);
	      xs = xs[2];
	  }
	  for (var i = arr.length; i--; ) {
	      acc = f(arr[i])(acc);
	  }
	  return acc;
        }
      }
    }
    function foldl1(f_49) {
      return function(_temp_50) {
        return function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var x_51 = v[1];
            var xs_52 = v[2];
            return foldl(f_49)(x_51)(xs_52)
          }
        }(_temp_50)
      }
    }
    function foldr1(f) {
      return function(xs) {
	if (xs[0] === "Nil") { throw "'foldr1' requires an non-empty list." }
	if (xs[0] !== "Cons") { throwError('foldr1'); }
	var arr = [];
	while (xs[0] === "Cons") {
	    arr.push(xs[1]);
	    xs = xs[2];
	}
        var acc = arr.pop();
	for (var i = arr.length; i--; ) {
	    acc = f(arr[i])(acc);
	}
	return acc;
      }
    }
    function scanl(f) {
      return function(b) {
        return function(xs) {
          if (xs[0] === "Nil") { return ["Cons",b,["Nil"]]; }
	  if (xs[0] !== "Cons") { throwError('scanl'); }
	  var arr = [b];
	  while (xs[0] === "Cons") {
	      b = f(xs[1])(b);
	      arr.push(b);
	      xs = xs[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
        }
      }
    }
    function scanl1(f) {
      return function(xs) {
	if (xs[0] !== "Cons") {
	    throw "Error: 'scanl1' requires a list of at least length 1.";
	}
	return scanl(f)(xs[1])(xs[2]);
      }
    }
    function filter(pred) {
      return function(xs) {
        if (xs[0] === "Nil") { return xs; }
	if (xs[0] !== "Cons") { throwError('filter'); }
	var arr = [];
	while (xs[0] === "Cons") {
          if (pred(xs[1])) { arr.push(xs[1]); }
	  xs = xs[2];
	}
	var out = ["Nil"];
	for (var i = arr.length; i--; ) {
	    out = ["Cons", arr[i], out];
	}
	return out;
      }
    }
    function concatMap(f_76) {
      return function(x) {
        return concat(map(f_76)(x))
      }
    }
    function all(pred) {
	return foldl(function(x) { return function(acc) {
		    return acc && pred(x);
		};})(true);
    }
    function any(pred) {
	return foldl(function(x) { return function(acc) {
		    return acc || pred(x);
		};})(false);
    }
    function partition(pred_93) {
      return function(lst_94) {
        return function() {
          var v = lst_94;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Tuple2", ["Nil"], ["Nil"]]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var x_95 = v[1];
              var xs_96 = v[2];
              return function(v) {
                if("Tuple2" !== v[0]) {
                  return undefined
                }else {
                  var as_97 = v[1];
                  var bs_98 = v[2];
                  return pred_93(x_95) ? ["Tuple2", ["Cons", x_95, as_97], bs_98] : ["Tuple2", as_97, ["Cons", x_95, bs_98]]
                }
              }(partition(pred_93)(xs_96))
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function zipWith(f) {
      return function(listA) {
	  return function(listB) {
	  if (listA[0] === "Nil" || listB[0] === "Nil") { return ["Nil"]; }
	  if (listA[0] !== "Cons" || listB[0] !== "Cons") { throwError('zipWith'); }
	  var arr = [];
	  while (listA[0] === "Cons" && listB[0] === "Cons") {
	      arr.push(f(listA[1])(listB[1]));
	      listA = listA[2];
	      listB = listB[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
        }
      }
    }
    function zip(listA) {
      return function(listB) {
	  if (listA[0] === "Nil" || listB[0] === "Nil") { return ["Nil"]; }
	  if (listA[0] !== "Cons" || listB[0] !== "Cons") { throwError('zip'); }
	  var arr = [];
	  while (listA[0] === "Cons" && listB[0] === "Cons") {
	      arr.push(["Tuple2", listA[1], listB[1]]);
	      listA = listA[2];
	      listB = listB[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
      }
    }
    function unzip(pairs_112) {
      return function() {
        var v = pairs_112;
        var c = [function(v) {
          if("Nil" !== v[0]) {
            return undefined
          }else {
            return["Tuple2", ["Nil"], ["Nil"]]
          }
        }, function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var p_113 = v[1];
            var ps_114 = v[2];
            return function(v) {
              if("Tuple2" !== v[0]) {
                return undefined
              }else {
                if("Tuple2" !== v[1][0]) {
                  return undefined
                }else {
                  var x_115 = v[1][1];
                  var y_116 = v[1][2];
                  if("Tuple2" !== v[2][0]) {
                    return undefined
                  }else {
                    var xs_117 = v[2][1];
                    var ys_118 = v[2][2];
                    return["Tuple2", ["Cons", x_115, xs_117], ["Cons", y_116, ys_118]]
                  }
                }
              }
            }(["Tuple2", p_113, unzip(ps_114)])
          }
        }];
        for(var i = c.length;i--;) {
          var r = c[i](v);
          if(r !== undefined) {
            return r
          }
        }
      }()
    }
    function intersperse(sep_119) {
      return function(xs_120) {
        return function() {
          var v = xs_120;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Nil"]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_124 = v[1];
              if("Nil" !== v[2][0]) {
                return undefined
              }else {
                return["Cons", a_124, ["Nil"]]
              }
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_121 = v[1];
              if("Cons" !== v[2][0]) {
                return undefined
              }else {
                var b_122 = v[2][1];
                var cs_123 = v[2][2];
                return["Cons", a_121, ["Cons", sep_119, intersperse(sep_119)(["Cons", b_122, cs_123])]]
              }
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function intercalate(sep_125) {
      return function(xs_126) {
        return function() {
          var v = xs_126;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Nil"]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_130 = v[1];
              if("Nil" !== v[2][0]) {
                return undefined
              }else {
                return a_130
              }
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_127 = v[1];
              if("Cons" !== v[2][0]) {
                return undefined
              }else {
                var b_128 = v[2][1];
                var cs_129 = v[2][2];
                return Value.append(a_127, Value.append(sep_125, intercalate(sep_125)(["Cons", b_128, cs_129])))
              }
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function sort(xs) {
      if (xs[0] === "Nil") { return xs; }
      if (xs[0] !== "Cons") { throwError('sort'); }
      var arr = [];
      while (xs[0] === "Cons") {
	  arr.push(xs[1]);
	  xs = xs[2];
      }
      arr.sort(function(a,b) { return a - b});
      var out = ["Nil"];
      for (var i = arr.length; i--; ) {
	  out = [ "Cons", arr[i], out ];
      }
      return out;
    }
    function take(n) { return function(xs) {
        if (n <= 0) { return ["Nil"]; }
	if (xs[0] === "Nil") { return xs; }
	if (xs[0] !== "Cons") { throwError('take'); }
	var out = [ "Cons", xs[1], ["Nil"] ];
	var temp = out;
	xs = xs[2];
	--n;
	while (xs[0] === "Cons" && n > 0) {
	    temp[2] = [ "Cons", xs[1], ["Nil"] ];
	    temp = temp[2];
	    xs = xs[2];
	    --n;
	}
	return out;
      };
    }
    function drop(n) { return function(xs) {
	if (xs[0] === "Nil") { return xs; }
	if (xs[0] !== "Cons") { throwError('drop'); }
	while (xs[0] === "Cons" && n > 0) {
	    xs = xs[2];
	    --n;
	}
	return xs;
      };
    }
    return {head:head,
	    tail:tail,
	    last:last,
	    map:map,
	    foldl:foldl,
	    foldr:foldr,
	    foldl1:foldl1,
	    foldr1:foldr1,
	    scanl:scanl,
	    scanl1:scanl1,
	    filter:filter,
	    length:length,
	    reverse:reverse,
	    concat:concat,
	    concatMap:concatMap,
	    and:and,
	    or:or,
	    all:all,
	    any:any,
	    sum:sum,
	    product:product,
	    maximum:maximum,
	    minimum:minimum,
	    partition:partition,
	    zipWith:zipWith,
	    zip:zip,
	    unzip:unzip,
	    intersperse:intersperse,
	    intercalate:intercalate,
	    sort:sort,
	    take:take,
	    drop:drop};
}();try{if(Elm.Dict)throw "Module name collision, 'Dict' is already defined.";Elm.Dict=function(){try{if(!(Elm.Prelude instanceof Object))throw 'module not found'}catch(e){throw "Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var hiddenVars=[];for(var i in Elm.Prelude){if(hiddenVars.indexOf(i)>=0)continue;this[i]=Elm.Prelude[i]};try{if(!(Elm.Data.Maybe instanceof Object))throw 'module not found'}catch(e){throw "Module 'Data.Maybe' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var isJust=Elm.Data.Maybe.isJust,Red_0=["Red"],Black_1=["Black"]
function RBNode_2(a1){return function(a2){return function(a3){return function(a4){return function(a5){return ["RBNode",a1,a2,a3,a4,a5]}}}}};var RBEmpty_3=["RBEmpty"],raise_4=console.log,empty_5=RBEmpty_3
function equal_pathLen_6(t_46){return function(){function path_numBlacks_47(t_48){return function(){switch(t_48[0]){case"RBEmpty":return 1;case"RBNode":return function(){var bl_52=path_numBlacks_47(t_48[4]),br_53=path_numBlacks_47(t_48[5]);return((not(eq(bl_52,br_53))||(eq(bl_52,-1)||eq(br_53,-1)))?-1:(bl_52+(eq(t_48[1],Red_0)?0:1)))}()};throw "Non-exhaustive pattern match in case"}()};return not(eq(-1,path_numBlacks_47(t_46)))}()}
function rootBlack_7(t_54){return function(){switch(t_54[0]){case"RBEmpty":return true;case"RBNode":switch(t_54[1][0]){case"Black":return true};break};return false}()}
function redBlack_children_8(t_55){return function(){switch(t_55[0]){case"RBEmpty":return true;case"RBNode":switch(t_55[1][0]){case"Red":switch(t_55[4][0]){case"RBNode":switch(t_55[4][1][0]){case"Red":return false};break};switch(t_55[5][0]){case"RBNode":switch(t_55[5][1][0]){case"Red":return false};break};break};return(redBlack_children_8(t_55[4])&&redBlack_children_8(t_55[5]))};throw "Non-exhaustive pattern match in case"}()}
function findExtreme_9(f_58){return function(t_59){return function(){switch(t_59[0]){case"RBEmpty":return Nothing;case"RBNode":return function(){var case6=findExtreme_9(f_58)(f_58(["Tuple2",t_59[4],t_59[5]]));switch(case6[0]){case"Just":return Just(case6[1]);case"Nothing":return Just(t_59[2])};throw "Non-exhaustive pattern match in case"}()};throw "Non-exhaustive pattern match in case"}()}}
function findminRbt_10(t_65){return findExtreme_9(fst)(t_65)}
function findmaxRbt_11(t_66){return findExtreme_9(snd)(t_66)}
function optionRelation_12(f_67){return function(u_68){return function(xo_69){return function(yo_70){return function(){var case0=["Tuple2",xo_69,yo_70];switch(case0[0]){case"Tuple2":switch(case0[1][0]){case"Nothing":return u_68};switch(case0[2][0]){case"Nothing":return u_68};switch(case0[1][0]){case"Just":switch(case0[2][0]){case"Just":return f_67(case0[1][1])(case0[2][1])};break};break};throw "Non-exhaustive pattern match in case"}()}}}}
function olt_13(xo_73){return function(yo_74){return optionRelation_12(function(x_75){return function(y_76){return(compare(x_75)(y_76)[0]==='LT')}})(true)(xo_73)(yo_74)}}
function olte_14(xo_77){return function(yo_78){return optionRelation_12(function(x_79){return function(y_80){return function(){var ord=compare(x_79)(y_80)[0];return ord==='LT'||ord==='EQ'}()}})(true)(xo_77)(yo_78)}}
function ordered_15(t_81){return function(){switch(t_81[0]){case"RBEmpty":return true;case"RBNode":return function(){var Tuple2$lmaxrmin_87=["Tuple2",findmaxRbt_11(t_81[4]),findminRbt_10(t_81[5])],lmax_88=function(){switch(Tuple2$lmaxrmin_87[0]){case"Tuple2":return Tuple2$lmaxrmin_87[1]};throw "Non-exhaustive pattern match in case"}(),rmin_89=function(){switch(Tuple2$lmaxrmin_87[0]){case"Tuple2":return Tuple2$lmaxrmin_87[2]};throw "Non-exhaustive pattern match in case"}();return(olte_14(lmax_88)(Just(t_81[2]))&&(olte_14(Just(t_81[2]))(rmin_89)&&(ordered_15(t_81[4])&&ordered_15(t_81[5]))))}()};throw "Non-exhaustive pattern match in case"}()}
function leftLeaning_16(t_94){return function(){switch(t_94[0]){case"RBEmpty":return true;case"RBNode":switch(t_94[4][0]){case"RBEmpty":switch(t_94[5][0]){case"RBNode":switch(t_94[5][1][0]){case"Red":return false};break};break;case"RBNode":switch(t_94[4][1][0]){case"Black":switch(t_94[5][0]){case"RBNode":switch(t_94[5][1][0]){case"Red":return false};break};break};break};return(leftLeaning_16(t_94[4])&&leftLeaning_16(t_94[5]))};throw "Non-exhaustive pattern match in case"}()}
function invariants_hold_17(t_97){return(ordered_15(t_97)&&(rootBlack_7(t_97)&&(redBlack_children_8(t_97)&&(equal_pathLen_6(t_97)&&leftLeaning_16(t_97)))))}
function min_18(t_98){return function(){switch(t_98[0]){case"RBEmpty":return console.log(Value.str("(min RBEmpty) is not defined"));case"RBNode":switch(t_98[4][0]){case"RBEmpty":return ["Tuple2",t_98[2],t_98[3]]};return min_18(t_98[4])};throw "Non-exhaustive pattern match in case"}()}
function max_19(t_102){return function(){switch(t_102[0]){case"RBEmpty":return console.log(Value.str("(max RBEmpty) is not defined"));case"RBNode":switch(t_102[5][0]){case"RBEmpty":return ["Tuple2",t_102[2],t_102[3]]};return max_19(t_102[5])};throw "Non-exhaustive pattern match in case"}()}
function lookup_20(k_106){return function(t_107){return function(){switch(t_107[0]){case"RBEmpty":return Nothing;case"RBNode":return function(){var case6=compare(k_106)(t_107[2]);switch(case6[0]){case"EQ":return Just(t_107[3]);case"GT":return lookup_20(k_106)(t_107[5]);case"LT":return lookup_20(k_106)(t_107[4])};throw "Non-exhaustive pattern match in case"}()};throw "Non-exhaustive pattern match in case"}()}}
function member_21(k_112){return function(t_113){return isJust(lookup_20(k_112)(t_113))}}
function rotateLeft_22(t_114){return function(){switch(t_114[0]){case"RBNode":switch(t_114[5][0]){case"RBNode":return RBNode_2(t_114[1])(t_114[5][2])(t_114[5][3])(RBNode_2(Red_0)(t_114[2])(t_114[3])(t_114[4])(t_114[5][4]))(t_114[5][5])};break};return raise_4(Value.str("rotateLeft of a node without enough children"))}()}
function rotateRight_23(t_124){return function(){switch(t_124[0]){case"RBNode":switch(t_124[4][0]){case"RBNode":return RBNode_2(t_124[1])(t_124[4][2])(t_124[4][3])(t_124[4][4])(RBNode_2(Red_0)(t_124[2])(t_124[3])(t_124[4][5])(t_124[5]))};break};return raise_4(Value.str("rotateRight of a node without enough children"))}()}
function rotateLeftIfNeeded_24(t_134){return function(){switch(t_134[0]){case"RBNode":switch(t_134[5][0]){case"RBNode":switch(t_134[5][1][0]){case"Red":return rotateLeft_22(t_134)};break};break};return t_134}()}
function rotateRightIfNeeded_25(t_135){return function(){switch(t_135[0]){case"RBNode":switch(t_135[4][0]){case"RBNode":switch(t_135[4][1][0]){case"Red":switch(t_135[4][4][0]){case"RBNode":switch(t_135[4][4][1][0]){case"Red":return rotateRight_23(t_135)};break};break};break};break};return t_135}()}
function otherColor_26(c_136){return function(){switch(c_136[0]){case"Black":return Red_0;case"Red":return Black_1};throw "Non-exhaustive pattern match in case"}()}
function color_flip_27(t_137){return function(){switch(t_137[0]){case"RBNode":switch(t_137[4][0]){case"RBNode":switch(t_137[5][0]){case"RBNode":return RBNode_2(otherColor_26(t_137[1]))(t_137[2])(t_137[3])(RBNode_2(otherColor_26(t_137[4][1]))(t_137[4][2])(t_137[4][3])(t_137[4][4])(t_137[4][5]))(RBNode_2(otherColor_26(t_137[5][1]))(t_137[5][2])(t_137[5][3])(t_137[5][4])(t_137[5][5]))};break};break};return raise_4(Value.str("color_flip called on a RBEmpty or RBNode with a RBEmpty child"))}()}
function color_flipIfNeeded_28(t_151){return function(){switch(t_151[0]){case"RBNode":switch(t_151[4][0]){case"RBNode":switch(t_151[4][1][0]){case"Red":switch(t_151[5][0]){case"RBNode":switch(t_151[5][1][0]){case"Red":return color_flip_27(t_151)};break};break};break};break};return t_151}()}
function fixUp_29(t_152){return color_flipIfNeeded_28(rotateRightIfNeeded_25(rotateLeftIfNeeded_24(t_152)))}
function ensureBlackRoot_30(t_153){return function(){switch(t_153[0]){case"RBNode":switch(t_153[1][0]){case"Red":return RBNode_2(Black_1)(t_153[2])(t_153[3])(t_153[4])(t_153[5])};break};return t_153}()}
function insert_31(k_158){return function(v_159){return function(t_160){return function(){function ins_161(t_162){return function(){switch(t_162[0]){case"RBEmpty":return RBNode_2(Red_0)(k_158)(v_159)(RBEmpty_3)(RBEmpty_3);case"RBNode":return function(){var h_168=function(){var case6=compare(k_158)(t_162[2]);switch(case6[0]){case"EQ":return RBNode_2(t_162[1])(t_162[2])(v_159)(t_162[4])(t_162[5]);case"GT":return RBNode_2(t_162[1])(t_162[2])(t_162[3])(t_162[4])(ins_161(t_162[5]));case"LT":return RBNode_2(t_162[1])(t_162[2])(t_162[3])(ins_161(t_162[4]))(t_162[5])};throw "Non-exhaustive pattern match in case"}();return fixUp_29(h_168)}()};throw "Non-exhaustive pattern match in case"}()};return(not(invariants_hold_17(t_160))?raise_4(Value.str("invariants broken before insert")):function(){var new_t_169=ensureBlackRoot_30(ins_161(t_160));return(not(invariants_hold_17(new_t_169))?raise_4(Value.str("invariants broken after insert")):new_t_169)}())}()}}}
function singleton_32(k_170){return function(v_171){return insert_31(k_170)(v_171)(RBEmpty_3)}}
function isRed_33(t_172){return function(){switch(t_172[0]){case"RBNode":switch(t_172[1][0]){case"Red":return true};break};return false}()}
function isRedLeft_34(t_173){return function(){switch(t_173[0]){case"RBNode":switch(t_173[4][0]){case"RBNode":switch(t_173[4][1][0]){case"Red":return true};break};break};return false}()}
function isRedLeftLeft_35(t_174){return function(){switch(t_174[0]){case"RBNode":switch(t_174[4][0]){case"RBNode":switch(t_174[4][4][0]){case"RBNode":switch(t_174[4][4][1][0]){case"Red":return true};break};break};break};return false}()}
function isRedRight_36(t_175){return function(){switch(t_175[0]){case"RBNode":switch(t_175[5][0]){case"RBNode":switch(t_175[5][1][0]){case"Red":return true};break};break};return false}()}
function isRedRightLeft_37(t_176){return function(){switch(t_176[0]){case"RBNode":switch(t_176[5][0]){case"RBNode":switch(t_176[5][4][0]){case"RBNode":switch(t_176[5][4][1][0]){case"Red":return true};break};break};break};return false}()}
function moveRedLeft_38(t_177){return function(){var t__178=color_flip_27(t_177);return function(){switch(t__178[0]){case"RBNode":return function(){switch(t__178[5][0]){case"RBNode":switch(t__178[5][4][0]){case"RBNode":switch(t__178[5][4][1][0]){case"Red":return color_flip_27(rotateLeft_22(RBNode_2(t__178[1])(t__178[2])(t__178[3])(t__178[4])(rotateRight_23(t__178[5]))))};break};break};return t__178}()};return t__178}()}()}
function moveRedRight_39(t_184){return function(){var t__185=color_flip_27(t_184);return(isRedLeftLeft_35(t__185)?color_flip_27(rotateRight_23(t__185)):t__185)}()}
function moveRedLeftIfNeeded_40(t_186){return((not(isRedLeft_34(t_186))&&not(isRedLeftLeft_35(t_186)))?moveRedLeft_38(t_186):t_186)}
function moveRedRightIfNeeded_41(t_187){return((not(isRedRight_36(t_187))&&not(isRedRightLeft_37(t_187)))?moveRedRight_39(t_187):t_187)}
function deleteMin_42(t_188){return function(){function del_189(t_190){return function(){switch(t_190[0]){case"RBNode":switch(t_190[4][0]){case"RBEmpty":return RBEmpty_3};break};return function(){var t__191=moveRedLeftIfNeeded_40(t_190);return function(){switch(t__191[0]){case"RBEmpty":return RBEmpty_3;case"RBNode":return fixUp_29(RBNode_2(t__191[1])(t__191[2])(t__191[3])(del_189(t__191[4]))(t__191[5]))};throw "Non-exhaustive pattern match in case"}()}()}()};return ensureBlackRoot_30(del_189(t_188))}()}
function deleteMax_43(t_197){return function(){function del_198(t_199){return function(){var t__200=(isRedLeft_34(t_199)?rotateRight_23(t_199):t_199);return function(){switch(t__200[0]){case"RBNode":switch(t__200[5][0]){case"RBEmpty":return RBEmpty_3};break};return function(){var t___201=moveRedRightIfNeeded_41(t__200);return function(){switch(t___201[0]){case"RBEmpty":return RBEmpty_3;case"RBNode":return fixUp_29(RBNode_2(t___201[1])(t___201[2])(t___201[3])(t___201[4])(del_198(t___201[5])))};throw "Non-exhaustive pattern match in case"}()}()}()}()};return ensureBlackRoot_30(del_198(t_197))}()}
function remove_44(k_207){return function(t_208){return function(){function eq_and_noRightNode_209(t_215){return function(){switch(t_215[0]){case"RBNode":switch(t_215[5][0]){case"RBEmpty":return eq(k_207,t_215[2])};break};return false}()}
function eq_210(t_217){return function(){switch(t_217[0]){case"RBNode":return eq(k_207,t_217[2])};return false}()}
function delLT_211(t_219){return function(){var t__220=moveRedLeftIfNeeded_40(t_219);return function(){switch(t__220[0]){case"RBEmpty":return raise_4(Value.str("delLT on RBEmpty"));case"RBNode":return fixUp_29(RBNode_2(t__220[1])(t__220[2])(t__220[3])(del_214(t__220[4]))(t__220[5]))};throw "Non-exhaustive pattern match in case"}()}()}
function delEQ_212(t_226){return function(){switch(t_226[0]){case"RBEmpty":return raise_4(Value.str("delEQ called on a RBEmpty"));case"RBNode":return function(){var Tuple2$k_v__230=min_18(t_226[5]),k__231=function(){switch(Tuple2$k_v__230[0]){case"Tuple2":return Tuple2$k_v__230[1]};throw "Non-exhaustive pattern match in case"}(),v__232=function(){switch(Tuple2$k_v__230[0]){case"Tuple2":return Tuple2$k_v__230[2]};throw "Non-exhaustive pattern match in case"}();return fixUp_29(RBNode_2(t_226[1])(k__231)(v__232)(t_226[4])(deleteMin_42(t_226[5])))}()};throw "Non-exhaustive pattern match in case"}()}
function delGT_213(t_237){return function(){switch(t_237[0]){case"RBEmpty":return raise_4(Value.str("delGT called on a RBEmpty"));case"RBNode":return fixUp_29(RBNode_2(t_237[1])(t_237[2])(t_237[3])(t_237[4])(del_214(t_237[5])))};throw "Non-exhaustive pattern match in case"}()}
function del_214(t_243){return function(){switch(t_243[0]){case"RBEmpty":return RBEmpty_3;case"RBNode":return((compare(k_207)(t_243[2])[0]==='LT')?delLT_211(t_243):function(){var t__245=(isRedLeft_34(t_243)?rotateRight_23(t_243):t_243);return(eq_and_noRightNode_209(t__245)?RBEmpty_3:function(){var t_246=moveRedRightIfNeeded_41(t_246);return(eq_210(t_246)?delEQ_212(t_246):delGT_213(t_246))}())}())};throw "Non-exhaustive pattern match in case"}()};return(not(invariants_hold_17(t_208))?raise_4(Value.str("invariants broken before remove")):function(){var t__247=ensureBlackRoot_30(del_214(t_208));return(invariants_hold_17(t__247)?t__247:raise_4(Value.str("invariants broken after remove")))}())}()}}
function fold_45(f_248){return function(acc_249){return function(t_250){return function(){switch(t_250[0]){case"RBEmpty":return acc_249;case"RBNode":return fold_45(f_248)(f_248(t_250[2])(t_250[3])(fold_45(f_248)(acc_249)(t_250[4])))(t_250[5])};throw "Non-exhaustive pattern match in case"}()}}};return {empty:empty_5,lookup:lookup_20,member:member_21,insert:insert_31,singleton:singleton_32,remove:remove_44,fold:fold_45}}();Elm.main=function(){return Elm.Dict.main}}catch(e){Elm.main=function(){var msg=('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>'+'<br/><span style="color:grey">Runtime Error in Dict module:<br/>'+e+'<br/><br/>The problem may stem from an improper usage of:<br/>EQ, GT, LT, console.log, fst, snd</span>');document.body.innerHTML=Text.monospace(msg);throw e}}try{if(Elm.Set)throw "Module name collision, 'Set' is already defined.";Elm.Set=function(){try{if(!(Elm.Prelude instanceof Object))throw 'module not found'}catch(e){throw "Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var hiddenVars=[];for(var i in Elm.Prelude){if(hiddenVars.indexOf(i)>=0)continue;this[i]=Elm.Prelude[i]};try{if(!(Elm.Dict instanceof Object))throw 'module not found'}catch(e){throw "Module 'Dict' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var Dict=Elm.Dict,empty_0=Dict.empty,remove_3=Dict.remove,member_4=Dict.member
function singleton_1(k_5){return Dict.singleton(k_5)(["Tuple0"])}
function insert_2(k_6){return Dict.insert(k_6)(["Tuple0"])};return {empty:empty_0,singleton:singleton_1,insert:insert_2,remove:remove_3,member:member_4}}();Elm.main=function(){return Elm.Set.main}}catch(e){Elm.main=function(){var msg=('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>'+'<br/><span style="color:grey">Runtime Error in Set module:<br/>'+e+'<br/><br/>The problem may stem from an improper usage of:<br/>Dict.empty, Dict.insert, Dict.member, Dict.remove, Dict.singleton</span>');document.body.innerHTML=Text.monospace(msg);throw e}}
var Elm = Elm || {};
Elm.Char = function() {
    function isBetween(lo,hi) { return function(chr) {
	    var c = chr.charCodeAt(0);
	    return lo <= c && c <= hi;
	};
    }
    var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
    var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
    var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));
    
    return {fromCode : function(c) { return String.fromCharCode(c); },
	    toCode : function(c) { return c.charCodeAt(0); },
	    toUpper : function(c) { return c.toUpperCase(); },
	    toLower : function(c) { return c.toLowerCase(); },
	    toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
	    toLocaleLower : function(c) { return c.toLocaleLowerCase(); },
	    isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
	    isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
	    isDigit    : isDigit,
	    isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
	    isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
    };
}();

var Elm = Elm || {};
Elm.Maybe = function() {
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
	    catMaybes : Elm.List.foldr(consMaybe)(["Nil"]),
	    isJust : function(m) { return m[0] === "Just"; },
	    isNothing : function(m) { return m[0] === "Nothing"; },
	    fromMaybe : fromMaybe,
	    consMaybe : consMaybe,
	    mapMaybe : function(f) { return Elm.List.foldr(mapCons(f))(["Nil"]); },
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

var String = {toText:Value.toText, properEscape:Value.properEscape};
var Elm = Elm || {};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Color = function() {
  function Color_0(a1) {
    return function(a2) {
      return function(a3) {
        return function(a4) {
          return["Color", a1, a2, a3, a4]
        }
      }
    }
  }
  var rgba_1 = Color_0;
  var red_3 = ["Color",255,0,0,1];
  var green_4 = ["Color",0,255,0,1];
  var blue_5 = ["Color",0,0,255,1];
  var yellow_6 = ["Color",255,255,0,1];
  var cyan_7 = ["Color",0,255,255,1];
  var magenta_8 = ["Color",255,0,255,1];
  var black_9 = ["Color",0,0,0,1];
  var white_10 = ["Color",255,255,255,1];
  var gray_11 = ["Color",128,128,128,1];
  var grey_12 = ["Color",128,128,128,1];
  function rgb_2(r_13) {
    return function(g_14) {
      return function(b_15) {
        return ["Color",r_13,g_14,b_15,1]
      }
    }
  }
  function extract(c) {
      if (c[4] === 1) { return 'rgb(' + c[1] + ',' + c[2] + ',' + c[3] + ')'; }
      return 'rgba(' + c[1] + ',' + c[2] + ',' + c[3] + ',' + c[4] + ')';
  }
  return{rgba:rgba_1, rgb:rgb_2, red:red_3, green:green_4, blue:blue_5, yellow:yellow_6, cyan:cyan_7, magenta:magenta_8, black:black_9, white:white_10, gray:gray_11, grey:grey_12,extract:extract}
}();
var Collage = function() {

var JS = Elm.JavaScript;

function tracePoints(ctx,points) {
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i][1], points[i][2]);
    while (i--) { ctx.lineTo(points[i][1], points[i][2]); }
}

function solid(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.strokeStyle = Elm.Graphics.Color.extract(color);
    ctx.stroke();
};

function filled(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.fillStyle = Elm.Graphics.Color.extract(color);
    ctx.fill();
}

function textured(redo,ctx,src,points) {
    var img = new Image();
    img.src = JS.castStringToJSString(src);
    img.onload = redo;
 
    tracePoints(ctx,points);
    ctx.fillStyle = ctx.createPattern(img,'repeat');
    ctx.fill();
}

function customLine(pattern,ctx,color,points) {
    if (pattern.length === 0) { pattern = [8,4]; }
    customLineHelp(ctx, pattern, points);
    ctx.strokeStyle = Elm.Graphics.Color.extract(color);
    ctx.stroke();
};

var customLineHelp = function(ctx, pattern, points) {
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i][1], y0 = points[i][2];
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
	x1 = points[i][1]; y1 = points[i][2];
	dx = x1 - x0; dy = y1 - y0;
	remaining = Math.sqrt(dx * dx + dy * dy);
	while (segmentLength <= remaining) {
	    x0 += dx * segmentLength / remaining;
	    y0 += dy * segmentLength / remaining;
	    ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
	    // update starting position
	    dx = x1 - x0; dy = y1 - y0;
	    remaining = Math.sqrt(dx * dx + dy * dy);
	    // update pattern
	    draw = !draw;
	    pindex = (pindex + 1) % plen;
	    segmentLength = pattern[pindex];
	}
	if (remaining > 0) {
	    ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
	    segmentLength -= remaining;
	}
	x0 = x1; y0 = y1;
    }
};

function drawLine(ctx,form) {
    var points = form[3][1];
    switch(form[1][0]) {
    case "Solid" : return solid(ctx,form[2],points);
    case "Dotted": return customLine([3,3],ctx,form[2],points);
    case "Dashed": return customLine([8,4],ctx,form[2],points);
    case "Custom": return customLine(form[1][1],ctx,form[2],points);
    }
};

function drawShape(redo,ctx,shapeStyle,color,points) {
    switch(shapeStyle[0]) {
    case "Filled":   return filled(ctx,color,points);
    case "Outlined": return solid(ctx,color,points);
    case "Textured": return textured(redo,ctx,shapeStyle[1],points);
    case "CustomOutline":
	return customLine(shapeStyle[1],ctx,color,points);
    }
};

function drawImage(redo,ctx,w,h,src) {
    var img = new Image();
    img.onload = redo;
    img.src = JS.castStringToJSString(src);
    ctx.drawImage(img,-w/2,-h/2,w,h);
}

function renderForm(redo,ctx,theta,scale,x,y,form) {
    ctx.save();
    if (x !== 0 || y !== 0) ctx.translate(x,y);
    if (theta !== ~~theta)  ctx.rotate(2*Math.PI*theta);
    if (scale !== 1)        ctx.scale(scale,scale);
    ctx.beginPath();
    switch(form[0]) {
    case "FLine":  drawLine(ctx,form); break;
    case "FShape": drawShape(redo,ctx,form[1],form[2],form[3][1]); break;
    case "FImage": drawImage(redo,ctx,form[1],form[2],form[3]); break;
    }
    ctx.restore();
};

function renderForms(redo,ctx,w,h,forms) {
    ctx.clearRect(0,0,w,h);
    for (var i = forms.length; i--; ) {
	var f = forms[i];
	renderForm(redo,ctx,f[1],f[2],f[3][1],f[3][2],f[4]);
    }
}

function collageForms(w,h,forms) {
    var canvas = Render.newElement('canvas');
    canvas.style.width  = (~~w) + 'px';
    canvas.style.height = (~~h) + 'px';
    canvas.style.display = "block";
    canvas.width  = ~~w;
    canvas.height = ~~h;
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	var w = canvas.width, h = canvas.height;
	function redo() { renderForms(this,ctx,w,h,forms); }
	renderForms(redo,ctx,w,h,forms);
	return canvas;
    }
    canvas.innerHTML = "Your browser does not support the canvas element.";
    return canvas;
};

function collageElement(w,h,theta,scale,x,y,elem) {
    var e = Render.render(elem);
    var t = "translate(" + (x - elem[3] / 2) + "px,"+ (y - elem[4] / 2) + "px)";
    var r = theta === (~~theta) ? "" : "rotate(" + theta*360 + "deg)";
    var s = scale === 1 ? "" : "scale(" + scale + "," + scale + ")";
    var transforms = t + " " + s + " " + r;
    e.style.transform       = transforms;
    e.style.msTransform     = transforms;
    e.style.MozTransform    = transforms;
    e.style.webkitTransform = transforms;
    e.style.OTransform      = transforms;
    var div = Render.newElement('div');
    Render.addTo(div,e);
    div.style.width = (~~w) + "px";
    div.style.height = (~~h) + "px";
    div.style.overflow = "hidden";
    return div;
}

function collage(w,h,formss) {
    if (formss.length === 0) { return collageForms(w,h,[]); }
    var elems = new Array(formss.length);
    for (var i = formss.length; i--; ) {
	var f = formss[i];
	if (typeof f[0] === "string") {
	    elems[i] = collageElement(w,h,f[1],f[2],f[3][1],f[3][2],f[4][1]);
	} else {
	    elems[i] = collageForms(w,h,f);
	}
    }
    if (formss.length === 1) { return elems[0]; }
    return Render.flowWith(Render.goIn,function(x){return x},elems);
}

function updateFormSet(node,currSet,nextSet) {
    if (Value.eq(nextSet,currSet)) return;
    var w = node.style.width.slice(0,-2) - 0;
    var h = node.style.height.slice(0,-2) - 0;
    if (typeof nextSet[0] === "object") {
	if (typeof currSet[0] === "object") {
	    if (node.getContext) {
		var ctx = node.getContext('2d');
		function redo() { renderForms(this,ctx,w,h,nextSet); }
		return renderForms(redo,ctx,w,h,nextSet);
	    }
	}
	var newNode = collageForms(w,h,nextSet);
	newNode.style.position = 'absolute';
	return node.parentNode.replaceChild(newNode,node);
    }
    var f = nextSet;
    var newNode = collageElement(w,h,f[1],f[2],f[3][1],f[3][2],f[4][1]);
    newNode.style.position = 'absolute';
    return node.parentNode.replaceChild(newNode,node);
}

// assumes that the form sets are the same length.
function updateCollage(node,currs,nexts) {
    if (nexts.length === 1) {
	return updateFormSet(node,currs[0],nexts[0]);
    }
    var kids = node.childNodes;
    var len = kids.length;
    for (var i = len; i--; ) {
	updateFormSet(kids[len-i-1], currs[i], nexts[i]);
    }
}

function style(clr,n,list) {
    return ["Tuple2",
	    '<span style="font-size:100%;color:' + clr + ';">' + n + '</span>',
	    list];
}

function insideForm(point) { return function(form) {
    if (!inBoundsOf(point[1],point[2],form)) return false;
    var hw, hh;
    switch (form[4][0]) {
    case "FShape": return insideShape(point,form[1],form[2],form[3],form[4][3][1]);
    case "FLine":  return false;
    case "FImage":
	hw = form[4][1] / 2;
	hh = form[4][2] / 2;
	break;
    case "FElement":
	hw = form[4][1][3] / 2;
	hh = form[4][1][4] / 2;
	break;
    }
    return insideShape(point,form[1],form[2],form[3],
		       [ [null, hw, hh],
			 [null,-hw, hh],
			 [null,-hw,-hh],
			 [null, hw,-hh],
			 [null, hw, hh] ]);
    };
}

function inBoundsOf(px,py,form) {
  if (form.length < 6) {
    var fx = form[3][1], fy = form[3][2];
    var radiusSquared = 0;
    var scale = form[2];
    switch (form[4][0]) {
    case "FShape":
      var points = form[4][3][1];
      for (var i = points.length; --i; ) {
	  var p = points[i];
	  radiusSquared = Math.max(radiusSquared, p[1]*p[1] + p[2]*p[2]);
      }
      radiusSquared *= scale * scale;
      break;
    case "FLine":
      break;
    case "FImage":
      var x = scale * form[4][1] / 2;
      var y = scale * form[4][2] / 2;
      radiusSquared = x*x + y*y;
      break;
    case "FElement":
      var x = scale * form[4][1][3] / 2;
      var y = scale * form[4][1][4] / 2;
      radiusSquared = x*x + y*y;
      break;
    }
    form.push(function(px,py) {
	    var dx = px - fx;
	    var dy = py - fy;
	    return dx*dx + dy*dy < radiusSquared + 1;
	});
  }
  return form[5](px,py);
}

function insideShape(point,theta,scale,pos,points) {
  var counter = 0;
  var list = ["Nil"];
  var p1,p2;

  var x = (point[1] - pos[1]) / scale;
  var y = (point[2] - pos[2]) / scale;
  if (theta !== 0) {
      var t = -2 * Math.PI * theta;
      var nx = x * Math.cos(t) - y * Math.sin(t);
      y = x * Math.sin(t) + y * Math.cos(t);
      x = nx;
  }

  if (points.length === 0) { return false; }
  p1 = points[0];
  for (var i = points.length - 1; i--; ) {
    p2 = points[i];
    var p1x = p1[1], p1y = p1[2], p2x = p2[1], p2y = p2[2];

    if (p1y < p2y) {var ymin=p1y, ymax=p2y;} else {var ymin=p2y, ymax=p1y;}
    if (p1x < p2x) {var xmin=p1x, xmax=p2x;} else {var xmin=p2x, xmax=p1x;}

    if (ymin < y && y <= ymax && x <= xmax) {
	if (x <= xmin || x <= ((y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x)) {
	    ++counter;
	}
    }
    p1 = p2;
  }
  return (counter % 2) === 1;
}

return {collage:collage, updateCollage:updateCollage, insideForm:insideForm};

}();var Elm = Elm || {};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = function() {
  var JS = Elm.JavaScript;
  var DLeft_0 = ["DLeft"];
  var DRight_1 = ["DRight"];
  var DUp_2 = ["DUp"];
  var DDown_3 = ["DDown"];
  var DIn_4 = ["DIn"];
  var DOut_5 = ["DOut"];
  function Absolute_12(a1) {
    return["Absolute", a1]
  }
  function Relative_13(a1) {
    return["Relative", a1]
  }
  var Near_14 = ["Near"];
  var Mid_15 = ["Mid"];
  var Far_16 = ["Far"];
  function Position_17(a1) {
    return function(a2) {
      return["Position", a1, a2]
    }
  }
  function PositionTL_18(a1) {
    return function(a2) {
      return["PositionTL", a1, a2]
    }
  }
  function PositionTR_19(a1) {
    return function(a2) {
      return["PositionTR", a1, a2]
    }
  }
  function PositionBL_20(a1) {
    return function(a2) {
      return["PositionBL", a1, a2]
    }
  }
  function PositionBR_21(a1) {
    return function(a2) {
      return["PositionBR", a1, a2]
    }
  }
  function Element_37(id,e,w,h,o,c,l) {
    return["Element", id, e, w, h, o, c, l ]
  }
  function EText_39(a1) {
    return function(a2) {
	return["EText", a1, a2]
    }
  }
  function EImage_40(a1) {
    return["EImage", JS.castStringToJSString(a1)]
  }
  function EVideo_41(a1) {
    return["EVideo", JS.castStringToJSString(a1)]
  }
  function EFittedImage_42(a1) {
    return["EFittedImage", JS.castStringToJSString(a1)]
  }
  function EFlow_43(a1) {
    return function(a2) {
      return["EFlow", a1, JS.castListToJSArray(a2)]
    }
  }
  function ECollage_44(a1) {
    return function(a2) {
      return function(a3) {
	  return["ECollage", a1, a2, Value.groupForms(a3)]
      }
    }
  }
  var EEmpty_45 = ["EEmpty"];
  function EContainer_46(a1) {
    return function(a2) {
      return["EContainer", a1, a2]
    }
  }
  var Solid_68 = ["Solid"];
  var Dotted_69 = ["Dotted"];
  var Dashed_70 = ["Dashed"];
  function Custom_71(a1) {
    return["Custom", JS.castListToJSArray(a1)]
  }
  var Filled_72 = ["Filled"];
  var Outlined_73 = ["Outlined"];
  function CustomOutline_74(a1) {
    return["CustomOutline", JS.castListToJSArray(a1)]
  }
  function Line_75(a1) {
    return["Line", JS.castListToJSArray(a1)]
  }
  function Shape_78(a1) {
    return function(a2) {
	var points = JS.castListToJSArray(a1);
	if (points.length > 0) { points.push(points[0]); }
	return["Shape", points, a2];
    }
  }
  function Form_84(a1) {
    return function(a2) {
      return function(a3) {
        return function(a4) {
          return["Form", a1, a2, a3, a4]
        }
      }
    }
  }
  function FLine_85(a1) {
    return function(a2) {
      return function(a3) {
        return["FLine", a1, a2, a3]
      }
    }
  }
  function FShape_86(a1) {
    return function(a2) {
      return function(a3) {
        return["FShape", a1, a2, a3]
      }
    }
  }
  function FImage_87(a1) {
    return function(a2) {
      return function(a3) {
          return["FImage", a1, a2, JS.castStringToJSString(a3)]
      }
    }
  }
  function FElement_88(a1) {
      return["FElement", a1]
  }
  var left_6 = DLeft_0;
  var right_7 = DRight_1;
  var down_8 = DDown_3;
  var up_9 = DUp_2;
  var inward_10 = DIn_4;
  var outward_11 = DOut_5;
  var topLeft_22 = Position_17(Near_14)(Near_14);
  var topRight_23 = Position_17(Far_16)(Near_14);
  var bottomLeft_24 = Position_17(Near_14)(Far_16);
  var bottomRight_25 = Position_17(Far_16)(Far_16);
  var midLeft_26 = Position_17(Near_14)(Mid_15);
  var midRight_27 = Position_17(Far_16)(Mid_15);
  var midTop_28 = Position_17(Mid_15)(Near_14);
  var midBottom_29 = Position_17(Mid_15)(Far_16);
  var middle_30 = Position_17(Mid_15)(Mid_15);
  function middleAt(a1) {
    return function(a2) {
      return["PositionAt", a1, a2]
    }
  }
  var topLeftAt_31 = PositionTL_18;
  var topRightAt_32 = PositionTR_19;
  var bottomLeftAt_33 = PositionBL_20;
  var bottomRightAt_34 = PositionBR_21;
  var absolute_35 = Absolute_12;
  var relative_36 = Relative_13;
  function newElement_38(e,w,h,o,c,l) { return Element_37(Guid.guid(),e,w,h,o,c,l); }
  function basicNewElement(e,w,h) { return Element_37(Guid.guid(),e,w,h,1,Nothing,Nothing); }
  var line_76 = Line_75;
  var polygon_79 = Shape_78;
  function sprite_96(src) {
    return function(w) {
      return function(h) {
        return function(pos) {
          return Form_84(0)(1)(pos)(FImage_87(w)(h)(src))
	}
      }
    }
  }
  function toForm_97(pos) {
    return function(e) {
      return Form_84(0)(1)(pos)(FElement_88(e))
    }
  }
  function width_47(w__101) {
    return function(e) {
      var be = e[2];
      switch(be[0]) {
      case "EImage":
      case "EVideo":
	  return newElement_38(e[2],w__101,e[4] * w__101 / e[3], e[5], e[6], e[7]);
      case "EText":
	  var p = Value.getTextSize(w__101,e[4],be[2]);
	  return newElement_38(e[2], w__101, p[1], e[5], e[6], e[7]);
      }
      return newElement_38(e[2], w__101, e[4], e[5], e[6], e[7]);
    }
  }
  function height_48(h__108) {
    return function(e) {
      var be = e[2];
      switch(be[0]) {
      case "EImage":
      case "EVideo":
	  return newElement_38(e[2], e[3] * h__108 / e[4], h__108, e[5], e[6], e[7]);
      }
      return newElement_38(e[2], e[3], h__108, e[5], e[6], e[7]);
    }
  }
  function size_49(w) {
    return function(h) {
      return function(e) {
        return newElement_38(e[2], w, h, e[5], e[6], e[7]);
      }
    }
  }
  function opacity_50(o) {
    return function(e) {
      return newElement_38(e[2], e[3], e[4], o, e[6], e[7]);
    }
  }
  function color_51(c) {
    return function(e) {
      return newElement_38(e[2], e[3], e[4], e[5], Just(c), e[7]);
    }
  }
  function link(lnk) {
    return function(e) {
	return newElement_38(e[2], e[3], e[4], e[5], e[6], Just(JS.castStringToJSString(lnk)));
    }
  }
  function widthOf_52(e)  { return ~~e[3]; }
  function heightOf_53(e) { return ~~e[4]; }
  function sizeOf_54(e)   { return["Tuple2", ~~e[3], ~~e[4]] }
  function text_56(txt) {
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("left")(txt), p[0], p[1])
  }
  function plainText(str) {
    var txt = Value.toText(str);
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("left")(txt),p[0],p[1])
  }
  function asText(v) {
    var txt = monospace(Value.toText(Value.show(v)));
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("left")(txt),p[0],p[1])
  }
  function centeredText(txt) {
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("center")(txt),p[0],p[1])
  }
  function justifiedText(txt) {
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("justify")(txt),p[0],p[1])
  }
  function rightedText(txt) {
    var p = Value.getTextSize(0,0,txt);
    return basicNewElement(EText_39("right")(txt),p[0],p[1])
  }
  function image_57(w) {
    return function(h) {
      return function(src) {
	  return basicNewElement(EImage_40(src),w,h)
      }
    }
  }
  function images(srcs) {
      var pics = Elm.Signal.constant(spacer_66(0)(0));
      var update = Elm.Signal.lift(function(src) {
	      src = JS.castStringToJSString(src);
	      var img = new Image();
	      img.onload = function() {
		  Dispatcher.notify(pics.id,
				    image_57(this.width)(this.height)(src));
	      };
	      img.src = src;
	  })(srcs);
      function f(x) { return function(y) { return x; } }
      var combine = Elm.Signal.lift2(f)(pics)(update);
      return combine;
  }
  function video_58(w) {
    return function(h) {
      return function(src) {
	  return basicNewElement(EVideo_41(src),w,h)
      }
    }
  }
  function fittedImage_59(w_147) {
    return function(h_148) {
      return function(s_149) {
	  return basicNewElement(EFittedImage_42(s_149),w_147,h_148)
      }
    }
  }
  function flow_60(dir_150) {
    return function(es_151) {
      return function() {
        var w_152 = function() {
          var ws_154 = map(widthOf_52)(es_151);
          return function(case1) {
            var case0 = case1;
            switch(case0[0]) {
              case "DLeft":
                return sum(ws_154);
              case "DRight":
                return sum(ws_154)
            }
            return maximum(ws_154)
          }(dir_150)
        }();
        var h_153 = function() {
          var hs_155 = map(heightOf_53)(es_151);
          return function(case3) {
            var case2 = case3;
            switch(case2[0]) {
              case "DDown":
                return sum(hs_155);
              case "DUp":
                return sum(hs_155)
            }
            return maximum(hs_155)
          }(dir_150)
        }();
        return basicNewElement(EFlow_43(dir_150)(es_151), w_152, h_153)
      }()
    }
  }
  function above_61(e1_156) {
    return function(e2_157) {
	return basicNewElement(EFlow_43(DDown_3)(["Cons", e1_156, ["Cons", e2_157, ["Nil"]]]), max(widthOf_52(e1_156))(widthOf_52(e2_157)), heightOf_53(e1_156) + heightOf_53(e2_157))
    }
  }
  function below_62(e1_158) {
    return function(e2_159) {
	return basicNewElement(EFlow_43(DDown_3)(["Cons", e2_159, ["Cons", e1_158, ["Nil"]]]), max(widthOf_52(e1_158))(widthOf_52(e2_159)), heightOf_53(e1_158) + heightOf_53(e2_159))
    }
  }
  function beside_63(e1_160) {
    return function(e2_161) {
	return basicNewElement(EFlow_43(DRight_1)(["Cons", e1_160, ["Cons", e2_161, ["Nil"]]]), widthOf_52(e1_160) + widthOf_52(e2_161), max(heightOf_53(e1_160))(heightOf_53(e2_161)))
    }
  }
  function layers_64(es_162) {
      return basicNewElement(EFlow_43(DOut_5)(es_162), maximum(map(widthOf_52)(es_162)), maximum(map(heightOf_53)(es_162)))
  }
  function collage_65(w_163) {
    return function(h_164) {
      return function(forms_165) {
	  return basicNewElement(ECollage_44(w_163)(h_164)(forms_165),w_163,h_164)
      }
    }
  }
  function spacer_66(w_166) {
    return function(h_167) {
	return basicNewElement(EEmpty_45,w_166,h_167)
    }
  }
  function container_67(w_169) {
    return function(h_170) {
      return function(pos_168) {
        return function(e_171) {
	    return basicNewElement(EContainer_46(pos_168)(e_171),w_169,h_170)
        }
      }
    }
  }
  function segment_77(p1_172) {
    return function(p2_173) {
      return Line_75(["Cons", p1_172, ["Cons", p2_173, ["Nil"]]])
    }
  }
  function rect_80(w_174) {
    return function(h_175) {
      return function(pos_176) {
        return Shape_78(["Cons", ["Tuple2", 0 - w_174 / 2, 0 - h_175 / 2],
			 ["Cons", ["Tuple2", 0 - w_174 / 2, h_175 / 2],
			  ["Cons", ["Tuple2", w_174 / 2, h_175 / 2],
			   ["Cons", ["Tuple2", w_174 / 2, 0 - h_175 / 2], ["Nil"]]]]])(pos_176)
      }
    }
  }
  function oval_81(w_177) {
    return function(h_178) {
      return function(pos_179) {
        return function() {
          var n_180 = 50;
          return function() {
            function f_181(i_182) {
              return["Tuple2", w_177 / 2 * cos(2 * (pi / n_180) * i_182), h_178 / 2 * sin(2 * (pi / n_180) * i_182)]
            }
            return Shape_78(map(f_181)(function() {
              var lo = 0;
              var hi = n_180 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  var lst = ["Cons", hi, lst]
                }while(hi-- > lo)
              }
              return lst
            }()))(pos_179)
          }()
        }()
      }
    }
  }
  function circle_82(r_183) {
    return oval_81(2 * r_183)(2 * r_183)
  }
  function ngon_83(n_184) {
    return function(r_185) {
      return function(pos_186) {
        return function() {
          var m_187 = toFloat(n_184);
          return function() {
            function f_188(i_189) {
              return["Tuple2", r_185 * cos(2 * (pi / m_187) * i_189), r_185 * sin(2 * (pi / m_187) * i_189)]
            }
            return Shape_78(map(f_188)(function() {
              var lo = 0;
              var hi = n_184 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  var lst = ["Cons", hi, lst]
                }while(hi-- > lo)
              }
              return lst
            }()))(pos_186)
          }()
        }()
      }
    }
  }
  function solid_89(clr_190) {
    return function(ln_191) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Solid_68)(clr_190)(ln_191))
    }
  }
  function dotted_90(clr_192) {
    return function(ln_193) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Dotted_69)(clr_192)(ln_193))
    }
  }
  function dashed_91(clr_194) {
    return function(ln_195) {
      return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Dashed_70)(clr_194)(ln_195))
    }
  }
  function customLine_92(pattern_196) {
    return function(clr_197) {
      return function(ln_198) {
        return Form_84(0)(1)(["Tuple2", 0, 0])(FLine_85(Custom_71(pattern_196))(clr_197)(ln_198))
      }
    }
  }
  function filled_93(clr) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(Filled_72)(clr)(shape));
    }
  }
  function outlined_94(clr) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(Outlined_73)(clr)(shape));
    }
  }
  function customOutline_95(pattern) {
    return function(clr) {
      return function(shape) {
	  return Form_84(0)(1)(shape[2])(FShape_86(CustomOutline_74(pattern))(clr)(shape));
      }
    }
  }
  function textured(src) {
    return function(shape) {
      return Form_84(0)(1)(shape[2])(FShape_86(["Textured",src])(null)(shape));
    }
  }
  function rotate_98(t_212) {
    return function(Form$thetascaleposform_213) {
      return function(case5) {
        var case0 = case5;
        switch(case0[0]) {
          case "Form":
            var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4];
            return Form_84(t_212 + case1)(case2)(case3)(case4)
        }
        throw"Non-exhaustive pattern match in case";
      }(Form$thetascaleposform_213)
    }
  }
  function scale_99(s) {
    return function(form) {
      return Form_84(form[1])(s * form[2])(form[3])(form[4])
    }
  }
  function move_100(x_224) {
    return function(y_225) {
      return function(Form$thetascaleTuple2$pxpyform_226) {
        return function(case7) {
          var case0 = case7;
          switch(case0[0]) {
            case "Form":
              var case1 = case0[1], case2 = case0[2], case3 = case0[3], case4 = case0[4];
              switch(case3[0]) {
                case "Tuple2":
                  var case5 = case3[1], case6 = case3[2];
                  return Form_84(case1)(case2)(["Tuple2", x_224 + case5, y_225 + case6])(case4)
              }
              break
          }
          throw"Non-exhaustive pattern match in case";
        }(Form$thetascaleTuple2$pxpyform_226)
      }
    }
  }
  return{left:left_6, right:right_7, down:down_8, up:up_9, inward:inward_10, outward:outward_11, topLeft:topLeft_22, topRight:topRight_23, bottomLeft:bottomLeft_24, bottomRight:bottomRight_25, midLeft:midLeft_26, midRight:midRight_27, midTop:midTop_28, midBottom:midBottom_29, middle:middle_30, middleAt:middleAt, topLeftAt:topLeftAt_31, topRightAt:topRightAt_32, bottomLeftAt:bottomLeftAt_33, bottomRightAt:bottomRightAt_34, absolute:absolute_35, relative:relative_36, width:width_47, height:height_48, size:size_49, opacity:opacity_50, 
	 color:color_51, link:link, widthOf:widthOf_52, heightOf:heightOf_53, sizeOf:sizeOf_54, text:text_56, asText:asText, plainText:plainText, centeredText:centeredText, justifiedText:justifiedText, rightedText:rightedText, image:image_57, images:images, video:video_58, fittedImage:fittedImage_59, flow:flow_60, above:above_61, below:below_62, beside:beside_63, layers:layers_64, collage:collage_65, spacer:spacer_66, container:container_67, line:line_76, segment:segment_77, polygon:polygon_79, rect:rect_80, oval:oval_81, circle:circle_82, ngon:ngon_83, solid:solid_89, dotted:dotted_90, dashed:dashed_91, customLine:customLine_92, filled:filled_93, 
	 outlined:outlined_94, customOutline:customOutline_95, textured:textured, sprite:sprite_96, toForm:toForm_97, rotate:rotate_98, scale:scale_99, move:move_100,
	 isWithin: Collage.insideForm}
}();
var Text = function() {
  function fromString(s) { return Value.toText(s); }

  var addTag = function(tag) { return function(text) {
	return '<' + tag + ' style="padding:0;margin:0">' + text + '</' + tag + '>';
    };
  };
  var addStyle = function(style, value) { return function(text) {
	return "<span style='" + style + ":" + value + "'>" + text + "</span>";
    };
  };

  var typeface = function(name) {
      name = Elm.JavaScript.castStringToJSString(name);
      return addStyle('font-family', name);
  };
  var size = function(px) {
    return addStyle('font-size', px + 'px');
  };
  var header = addTag('h1');
  var height = function(h) { return addStyle('font-size', h+'em'); }
  var italic = addStyle('font-style', 'italic');
  var bold = addTag('b');
  var color = function(c) {
    return addStyle('color', Elm.Graphics.Color.extract(c));
  };
  var underline = addStyle('text-decoration', 'underline');
  var overline = addStyle('text-decoration', 'overline');
  var strikeThrough = addStyle('text-decoration', 'line-through');
  var link = function(href) { return function(text) {
      return "<a href='" + fromString(href) + "'>" + text + "</a>";
    };
  };

  return {fromString : fromString,
	  toText: fromString,
	  header : header,
	  height : height,
	  italic : italic,
	  bold : bold,
	  underline : underline,
	  overline : overline,
	  strikeThrough : strikeThrough,
	  monospace : typeface('monospace'),
	  typeface : typeface,
	  color : color,
	  link : link };
}();

var Elm = Elm || {};
Elm.Graphics.Text = Text;

var Render = function(){

function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
};

function addTo(container, elem) {
    container.appendChild(elem);
};

function makeText(pos,txt) {
    var e = newElement('div');
    e.innerHTML = txt;
    e.style.textAlign = pos;
    return e;
};

function image(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function fittedImage(w,h,src) {
    var canvas = newElement('canvas');
    canvas.style.display = "block";
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.width  = w;
    canvas.height = h;
    canvas.innerHTML = "Your browser does not support the canvas element.";

    var img = newElement('img');
    img.onload = function() {
	if (canvas.getContext) {
	    var ctx = canvas.getContext('2d');
	    var sx = 0, sy = 0, sWidth = this.width, sHeight = this.height;
	    if (w / h > this.width / this.height) {
		sHeight = this.width * h / w;
		sy = (this.height - sHeight) / 2;
	    } else {
		sWidth = this.height * w / h;
		sx = (this.width - sWidth) / 2;
	    }
	    ctx.drawImage(img, sx, sy, sWidth, sHeight,
			  0,0, canvas.width, canvas.height);
	}
    };
    img.src = src;
    return canvas;
};

var video = function(src) {
    var e = newElement('video');
    e.controls = "controls";
    var source = newElement('source');
    source.src = src;
    var segs = src.split('.');
    source.type = "video/" + segs[segs.length-1];
    addTo(e, source);
    e.style.display = "block";
    return e;
};

function divify(e) {
    var div = newElement('div');
    addTo(div, e);
    return div;
};
function goDown(e) {
    return e //.tagName === "DIV" ? e : divify(e);
};
function goRight(e) {
    e.style.styleFloat = "left";
    e.style.cssFloat = "left";
    return e;
};
function goIn(e) {
    e.style.position = 'absolute';
    return e;
};
function flowWith(f, prep, elist) {
    var container = newElement('div');
    for (var i = elist.length; i--; ) {
	addTo(container, f(prep(elist[i])));
    }
    return container;
};

function flow(dir,elist) {
    switch(dir) {
    case "DDown":  elist = elist.slice(0).reverse();
    case "DUp":    return flowWith(goDown,render,elist);
    case "DRight": elist = elist.slice(0).reverse();
    case "DLeft":  return flowWith(goRight,render,elist);
    case "DOut":   elist = elist.slice(0).reverse();
    case "DIn":    return flowWith(goIn,render,elist);
    };
};

function toPos(pos) {
    switch(pos[0]) {
    case "Absolute": return  pos[1] + "px";
    case "Relative": return (pos[1] * 100) + "%";
    }
}

function setPos(pos,e) {
  e.style.position = 'absolute';
  e.style.margin = 'auto';
  switch(pos[0]) {
  case "Position":
      if (pos[1][0] !== "Far")  e.style.left = 0;
      if (pos[1][0] !== "Near") e.style.right = 0;
      if (pos[2][0] !== "Far")  e.style.top = 0;
      if (pos[2][0] !== "Near") e.style.bottom = 0;
      break;
  case "PositionAt":
      e.style.top  = toPos(pos[2]);
      e.style.left = toPos(pos[1]);
      var shift = "translate(" + (-elem[3]/2) + "px," + (-elem[4]/2) + "px)";
      e.style.transform       = shift;
      e.style.msTransform     = shift;
      e.style.MozTransform    = shift;
      e.style.webkitTransform = shift;
      e.style.OTransform      = shift;
      break;
  default:
      var p = pos[0].slice(-2);
      e.style[p[0] === "T" ? 'top' : 'bottom'] = toPos(pos[2]);
      e.style[p[1] === "L" ? 'left' : 'right'] = toPos(pos[1]);
  }
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos,e);
    var div = newElement('div');
    div.style.position = "relative";
    div.style.overflow = "hidden";
    addTo(div,e);
    return div;
};

function render(elem) {
    var e = {};
    switch(elem[2][0]) {
    case "EText":        e = makeText(elem[2][1],elem[2][2]); break;
    case "EImage":       e = image(elem[2][1]); break;
    case "EVideo":       e = video(elem[2][1]); break;
    case "EFittedImage": e = fittedImage(elem[3],elem[4],elem[2][1]); break;
    case "EFlow":        e = flow(elem[2][1][0],elem[2][2]); break;
    case "ECollage":     e = Collage.collage(elem[2][1],elem[2][2],elem[2][3]); break;
    case "EEmpty":       e = newElement('div'); break;
    case "EContainer":   e = container(elem[2][1],elem[2][2]); break;
    case "EHtml":
	e = elem[2][1];
	if (e.type !== 'button') {
	    var p = Value.getExcess(e);
	    elem[3] -= p[0];
	    elem[4] -= p[1];
	}
	break;
    case "EExternalHtml":
	e = newElement('div');
	addTo(e, elem[2][1]);
	break;
    }
    e.id = elem[1];
    e.style.width  = (~~elem[3]) + 'px';
    e.style.height = (~~elem[4]) + 'px';
    if (elem[5] !== 1) { e.style.opacity = elem[5]; }
    if (elem[6][0] === "Just") {
	e.style.backgroundColor = Elm.Graphics.Color.extract(elem[6][1]);
    }
    if (elem[7][0] === "Just") {
	var a = newElement('a');
	a.href = elem[7][1];
	addTo(a,e);
	return a;
    }
    return e;
};

function update(node,curr,next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr[1] === next[1]) return;
    if (curr[2][0] !== next[2][0]) {
	return node.parentNode.replaceChild(render(next),node);
    }
    var nextE = next[2], currE = curr[2];
    switch(nextE[0]) {
    case "EText":
	if (nextE[1] !== currE[1]) node.style.textAlign = nextE[1];
	if (nextE[2] !== currE[2]) node.innerHTML = nextE[2];
	break;
    case "EImage":
	if (nextE[1] !== currE[1]) node.src = nextE[1];
	break;
    case "EVideo":
    case "EFittedImage":
	if (!Value.eq(nextE,currE) || next[3]!==curr[3] || next[4]!==curr[4]) {
	    return node.parentNode.replaceChild(render(next),node);
	}
    break;
    case "ECollage":
	if (nextE[1] !== currE[1] || nextE[2] !== currE[2] || nextE[3].length !== currE[3].length) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	Collage.updateCollage(node,currE[3],nextE[3]);
	break;
    case "EFlow":
	if (nextE[1] !== currE[1]) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	var nexts = nextE[2];
	var kids = node.childNodes;
	if (nexts.length !== kids.length) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	var currs = currE[2];
	var goDir = {};
	switch(nextE[1][0]) {
	case "DDown":  case "DUp":   goDir = goDown; break;
	case "DRight": case "DLeft": goDir = goRight; break;
	case "DOut":   case "DIn":   goDir = goIn; break;
	}
	for (var i = kids.length; i-- ;) {
	    update(kids[i],currs[i],nexts[i]);
	    goDir(kids[i]);
	}
	break;
    case "EContainer":
	update(node.childNodes[0],currE[2],nextE[2]);
	setPos(nextE[1],node.childNodes[0]);
	break;
    case "EEmpty":
	break;
    case "EHtml":
	if (next[1] !== curr[1]) {
	    var e = render(next);
	    node.parentNode.replaceChild(e,node);
	    node = e;
	}
	if (e.type !== 'button') {
	    var p = Value.getExcess(node);
	    next[3] -= p[0];
	    next[4] -= p[1];
	}
	break;
    case "EExternalHtml":
	if (next[1] !== curr[1])
	    node.parentNode.replaceChild(render(next),node);
	break;
    }
    if (next[3] !== curr[3]) node.style.width   = (~~next[3]) + 'px';
    if (next[4] !== curr[4]) node.style.height  = (~~next[4]) + 'px';
    if (next[5] !== curr[5]) node.style.opacity = next[5];
    if (next[6].length === 2) {
	var clr = Elm.Graphics.Color.extract(next[6][1]);
	if (clr !== node.style.backgroundColor) node.style.backgroundColor = clr;
    }
    if (next[7].length === 2) {
	if (curr[7].length === 1 || next[7][1] !== curr[7][1]) node.parentNode.href = next[7][1];
    }
    next[1] = curr[1];
}

return {render:render,update:update,addTo:addTo,newElement:newElement,flowWith:flowWith,goIn:goIn};

}(); 
var Elm = Elm || {};
Elm.Signal = function() {
  var send = function(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  };
  var input = function(base) {
    this.id = Guid.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    Dispatcher.inputs.push(this);
  };
  var lift = function(func,args) {
    this.id = Guid.guid();
    this.value = null;
    this.kids = [];
    this.count = 0;
    this.changed = false;
    
    args.reverse();
    this.recalc = function() {
	var f = func;
	for (var i = args.length; i--; ) {
	    f = f(args[i].value);
	}
	this.value = f;
    };
    this.recalc();
    
    this.recv = function(timestep, changed, parentID) {
      this.count += 1;
      if (changed) { this.changed = true; }
      if (this.count == args.length) {
	  if (this.changed) { this.recalc() }
	  send(this, timestep, this.changed);
	  this.changed = false;
	  this.count = 0;
      }
    };
    for (var i = args.length; i--; ) {
      args[i].kids.push(this);
    }
  };
  var fold = function(func,base,baseIsFunc,input) {
    this.id = Guid.guid();
    this.value = baseIsFunc ? base(input.value) : base;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      if (changed) { this.value = func(input.value)(this.value); }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  };

  var dropIf = function(pred,base,input) {
    this.id = Guid.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
	var chng = changed && !pred(input.value);
	if (chng) { this.value = input.value; }
	send(this, timestep, chng);
    };
    input.kids.push(this);
  };
  var dropRepeats = function(input) {
      this.id = Guid.guid();
      this.value = input.value;
      this.kids = [];
      this.recv = function(timestep, changed, parentID) {
	  var chng = changed && !eq(this.value,input.value);
	  if (chng) { this.value = input.value; }
	  send(this, timestep, chng);
      };
      input.kids.push(this);
  };
  
  var dropWhen = function(s1) { return function(b) { return function(s2) {
    var pairs = new lift(function(x){return function(y){return [x,y];};},[s1,s2]);
    var dropped = new dropIf(function(p){return p[0];},[true,b],pairs);
    return new lift(function(p){return p[1];},[dropped]); }; };
  };

  var sampleOn = function(s1,s2) {
    this.id = Guid.guid();
    this.value = s2.value;
    this.kids = [];
    this.count = 0;
    this.changed = false;
    
    this.recv = function(timestep, changed, parentID) {
	if (parentID === s1.id) this.changed = changed;
	this.count += 1;
	if (this.count == 2) {
	    if (this.changed) { this.value = s2.value; }
	    send(this, timestep, this.changed);
	    this.count = 0;
	    this.changed = false;
	}
    };
    s1.kids.push(this);
    s2.kids.push(this);
  };
  
  return {
    constant : function(v) { return new input(v); },
    lift  : function(f){return function(e){return new lift(f,[e]);};},
    lift2 : function(f) { return function(e1) { return function(e2) {
		  return new lift(f, [e1,e2]); }; }; },
    lift3 : function(f) { return function(e1) { return function(e2) {
		  return function(e3){return new lift(f,[e1,e2,e3]);};};};},
    lift4 : function(f) { return function(e1) { return function(e2) {
		  return function(e3) { return function(e4) {
			  return new lift(f, [e1,e2,e3,e4]); }; }; }; }; },
    foldp : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,false,e); }; }; },
    foldp_ : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,true,e); }; }; },
    foldp1 : function(f) { return function(e) {
	      return new fold(f,function(x){return x;},true,e); }; },
    count : function(sig) {
	  var incr = function(_){return function(c){return c+1;};};
	  return new fold(incr,0,false,sig) },
    keepIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(function(x) { return !pred(x)},base,sig); }; }; },
    dropIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(pred,base,sig); }; }; },
    keepWhen : function(s) { return dropWhen(new lift(function(b){return !b;},[s])); },
    dropWhen : dropWhen,
    dropRepeats : function(s) { return new dropRepeats(s);},
    sampleOn : function(s1){return function(s2){return new sampleOn(s1,s2);};}
  };
}();
var Dispatcher = function() {
    var program = null;
    var timestep = 0;
    var inputs = [];
    var currentElement = null;

    var initialize = function() {
	program = Elm.main();
	if (!program.hasOwnProperty('recv')) {
	    program = Elm.Signal.constant(program);
	}

	currentElement = program.value;
	filterDeadInputs();

	var content = document.getElementById('content');
	content.appendChild(Render.render(currentElement));
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
	program = Elm.Signal.lift(function(value) {
		var content = document.getElementById('content');
		Render.update(content.firstChild,currentElement,value);
		currentElement = value;
		return value;
	    })(program);
    };
    var notify = function(id, v) {
	timestep += 1;
	//console.log(timestep);
	var hasListener = false;
	for (var i = inputs.length; i--; ) {
	    hasListener = inputs[i].recv(timestep, id, v) || hasListener;
	}
	return hasListener;
    };

    function isAlive(input) {
	if (!input.hasOwnProperty('defaultNumberOfKids')) return true;
	var len = input.kids.length;
	if (len == 0) return false;
	if (len > input.defaultNumberOfKids) return true;
	var alive = false;
	for (var i = len; i--; ) {
	    alive = alive || isAlive(input.kids[i]);
	}
	return alive;
    }
    function filterDeadInputs() {
	var temp = [];
	for (var i = inputs.length; i--; ) {
	    if (isAlive(inputs[i])) temp.push(inputs[i]);
	}
	inputs = temp;
    }

    return {initialize:initialize, notify:notify, inputs:inputs};
}();Elm.HTTP = function() {
  var JS = Elm.JavaScript;
  var toElmString = Elm.JavaScript.castJSStringToString;
  function request(verb) { return function(url) { return function(data) {
    return function(headers) {
	return {0 : "Request",
		length : 1,
		verb : JS.castStringToJSString(verb),
		url : JS.castStringToJSString(url),
		data : data === null ? null : JS.castStringToJSString(data),
		headers : headers }; }; }; };
  }
  function get(url) { return request("GET")(url)(null)(["Nil"]); }
  function post(url) { return function(data) {
	  return request("POST")(url)(data)(["Nil"]); }; }

  function sendReq(responses) { return function(req) {
    Dispatcher.notify(responses.id,["Waiting"]);
    var request = null;
    if (window.XMLHttpRequest) {
	  request = new XMLHttpRequest();
    } else if (window.ActiveXObject) {
	  request = new ActiveXObject("Microsoft.XMLHTTP");
    }
    request.onreadystatechange = function(e) {
	if (request.readyState === 4) {
	  Dispatcher.notify(
	  responses.id,
	  request.status === 200
	  ? ["Success", toElmString(request.responseText)]
	  : ["Failure", request.status, toElmString(request.statusText)]);
	}
    };
    request.open(req.verb, req.url, true);
    Elm.List.map(function(pair) {
	    request.setRequestHeader(
		JS.castStringToJSString(pair[1]),
		JS.castStringToJSString(pair[2]));
	  })(req.headers);
    request.send(req.data);
    return null;
   };
  }

  function send(requests) {
    var responses = Elm.Signal.constant(["Waiting"]);
    var sender = Elm.Signal.lift(sendReq(responses))(requests);
    function f(x) { return function(y) { return x; } }
    var combine = Elm.Signal.lift2(f)(responses)(sender);
    return combine;
  }

  return {get   : get,
	  post  : post,
	  request : request,
	  send  : send,
	  sendGet : function(urls){return send(Elm.Signal.lift(get)(urls));}
	  };
}();

Elm.Input = function() {
    var JS = Elm.JavaScript;
    var toElmString = Elm.JavaScript.castJSStringToString;
    var newTextInput = function(elem, ghostText) {
	elem.placeholder = JS.castStringToJSString(ghostText);
	var str = Elm.Signal.constant(["Nil"]);
	Value.addListener(elem, 'keyup', function(e) {
		Dispatcher.notify(str.id, toElmString(elem.value));
		elem.focus();
	    });
	elem.style.padding = "1px";
	return Value.Tuple(Value.wrap(elem), str);
    };
    var newElement = function(name) {
	var e = document.createElement(name);
	e.style.padding = "0";
	e.style.margin = "0";
	return e;
    };
    var textArea = function(cols) { return function(rows) {
	    var textarea = newElement('textarea');
	    textarea.rows = rows;
	    textarea.cols = cols;
	    return newTextInput(textarea, "");
	};
    };
    var textField = function(ghostText) {
	var field = newElement('input');
	field.type = 'text';
	return newTextInput(field, ghostText);
    };
    var password = function(ghostText) {
	var field = newElement('input');
	field.type = 'password';
	return newTextInput(field, ghostText);
    };
    var checkbox = function(checked) {
	var box = newElement('input');
	box.type = 'checkbox';
	box.checked = checked;
	var status = Elm.Signal.constant(checked);
	Value.addListener(box, 'change', function(e) {
		Dispatcher.notify(status.id, box.checked);
	    });
	return Value.Tuple(Value.wrap(box), status);
    };
    var dropDown = function(options) {
	var slct = newElement('select');
	var opts = [];
	while (options[0] === "Cons") {
	    var opt = newElement('option');
	    var str = Text.toText(options[1][1]);
	    opt.value = str;
	    opt.innerHTML = str;
	    slct.appendChild(opt);
	    opts.push(options[1][2]);
	    options = options[2];
	}
	var status = Elm.Signal.constant(opts[0]);
	Value.addListener(slct, 'change', function(e) {
		Dispatcher.notify(status.id, opts[slct.selectedIndex]);
	    });
	return Value.Tuple(Value.wrap(slct), status);
    };
    var stringDropDown = function(opts) {
	return dropDown(Elm.List.map (function(x) {return Value.Tuple(x,x);}) (opts));
    };
    var button = function(name) {
	var b = newElement('input');
	b.type = "button";
	b.value = JS.castStringToJSString(name);
	var press = Elm.Signal.constant(false);
	Value.addListener(b, 'click', function(e) {
		Dispatcher.notify(press.id, true);
		Dispatcher.notify(press.id, false);
	    });
	return Value.Tuple(Value.wrap(b),press);
    };
    return {textArea:textArea, textField:textField,
	    password:password, checkbox:checkbox,
	    dropDown:dropDown, stringDropDown:stringDropDown,
	    button:button};
}();

Elm.Keyboard = { Raw : function() {
  var keysDown = Elm.Signal.constant(["Nil"]);
  var charPressed = Elm.Signal.constant(["Nothing"]);
  function remove(x,xs) {
	if (xs[0] === "Nil") return xs;
	if (xs[1] === x) return xs[2];
	return ["Cons", xs[1], remove(x,xs[2])];
  }
  function has(x,xs) {
	while (xs[0] !== "Nil") {
	  if (xs[1] === x) return true;
	  xs = xs[2];
	}
	return false;
  }
  Value.addListener(document, 'keydown', function(e) {
	  if (has(e.keyCode, keysDown.value)) return;
	  var hasListener = Dispatcher.notify(keysDown.id, ["Cons", e.keyCode, keysDown.value]);
	  if (!hasListener)
		this.removeEventListener('keydown',arguments.callee,false);
	});
  Value.addListener(document, 'keyup', function(e) {
	  var codes = remove(e.keyCode, keysDown.value);
	  var hasListener = Dispatcher.notify(keysDown.id, codes);
	  if (!hasListener)
		this.removeEventListener('keyup',arguments.callee,false);
	});
  Value.addListener(window, 'blur', function(e) {
	  var hasListener = Dispatcher.notify(keysDown.id, ["Nil"]);
	  if (!hasListener)
		this.removeEventListener('blur',arguments.callee,false);
	});
  Value.addListener(document, 'keypress', function(e) {
	  var hasListener = Dispatcher.notify(charPressed.id, ["Just",e.charCode || e.keyCode]);
	  Dispatcher.notify(charPressed.id, ["Nothing"]);
	  if (!hasListener)
		this.removeEventListener('keypress',arguments.callee,false);
	});
  return {keysDown:keysDown,
	  charPressed:charPressed};
    }()
};
Elm.Mouse = function() {
  var position  = Elm.Signal.constant(Value.Tuple(0,0));
  position.defaultNumberOfKids = 2;

  var x = Elm.Signal.lift(function(p){return p[1];})(position);
  x.defaultNumberOfKids = 0;
  var y = Elm.Signal.lift(function(p){return p[2];})(position);
  y.defaultNumberOfKids = 0;

  var isDown    = Elm.Signal.constant(false);
  var isClicked = Elm.Signal.constant(false);
  var clicks = Elm.Signal.constant(Value.Tuple());
  
  function getXY(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) {
	posx = e.pageX;
	posy = e.pageY;
    } else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft +
	  document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop +
	  document.documentElement.scrollTop;
    }
    return Value.Tuple(posx, posy);
  }

  Value.addListener(document, 'click', function(e) {
	  var hasListener1 = Dispatcher.notify(isClicked.id, true);
	  var hasListener2 = Dispatcher.notify(clicks.id, Value.Tuple());
	  Dispatcher.notify(isClicked.id, false);
	  if (!hasListener1 && !hasListener2)
		this.removeEventListener('click',arguments.callee,false);
	});
  Value.addListener(document, 'mousedown', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, true);
	  if (!hasListener)
		this.removeEventListener('mousedown',arguments.callee,false);
	});
  Value.addListener(document, 'mouseup', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, false);
	  if (!hasListener)
		this.removeEventListener('mouseup',arguments.callee,false);
	});
  Value.addListener(document, 'mousemove', function(e) {
	  var hasListener = Dispatcher.notify(position.id, getXY(e));
	  if (!hasListener)
		this.removeEventListener('mousemove',arguments.callee,false);
	});
  var clickedOn = function(elem) {
	var node = Render.render(elem);
	var click = Elm.Signal.constant(false);
	Value.addListener(node, 'click', function(e) {
		Dispatcher.notify(click.id, true);
		Dispatcher.notify(click.id, false);
	  });
	return Value.Tuple(Value.wrap(node), click);
  };
  return {position: position,
	  x:x,
	  y:y,
	  isClicked: isClicked,
	  isDown: isDown,
	  clicks: clicks,
	  isClickedOn: clickedOn
	  };
  }();
Elm.Random = function() {
  var inRange = function(min) { return function(max) {
      return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
    };
  };
  var randomize = function(min) { return function(max) { return function(signal) {
      function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
      return Elm.Signal.lift(f)(signal);
    };
   };
  };
  return { inRange:inRange, randomize:randomize };
}();Elm.Time = function() {
  var every = function(t) {
      t *= 1000;
      var clock = Elm.Signal.constant(0);
      var time = 0;
      setInterval(function() {
	      time += t;
	      Dispatcher.notify(clock.id, time/1000);
	  }, t);
      return clock;
  };
  var after = function(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(false);
      setTimeout(function() { Dispatcher.notify(thread.id, true); }, t);
      return thread;
  };
  var before = function(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(true);
      setTimeout(function() { Dispatcher.notify(thread.id, false); }, t);
      return thread;
  };
  return {every:every,after:after,before:before};
}();Elm.Window = function() {
  var dimensions = Elm.Signal.constant(Value.Tuple(window.innerWidth,
						   window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  var width  = Elm.Signal.lift(function(p){return p[1];})(dimensions);
  width.defaultNumberOfKids = 0;
  var height = Elm.Signal.lift(function(p){return p[2];})(dimensions);
  height.defaultNumberOfKids = 0;

  Value.addListener(window, 'resize', function(e) {
	  var w = document.getElementById('widthChecker').offsetWidth;
	  var hasListener = Dispatcher.notify(dimensions.id,
					      Value.Tuple(w, window.innerHeight));
	  if (!hasListener)
		this.removeEventListener('resize',arguments.callee,false);
	});
  return {dimensions:dimensions,width:width,height:height};
}();

Value.addListener(document, 'elm_log', function(e) { console.log(e.value); });
Value.addListener(document, 'elm_title', function(e) {document.title = e.value;});
Value.addListener(document, 'elm_redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
    });

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
	    Just    : Elm.Maybe.Just,
	    Nothing : Elm.Maybe.Nothing,
	    maybe   : Elm.Maybe.maybe,
	    map     : Elm.List.map,
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
	    foldp : Elm.Signal.foldp,
	    foldp1 : Elm.Signal.foldp1,
	    foldp_ : Elm.Signal.foldp_,
	    constant : Elm.Signal.constant,
	    count : Elm.Signal.count,
	    keepIf : Elm.Signal.keepIf,
	    dropIf : Elm.Signal.dropIf,
	    keepWhen : Elm.Signal.keepWhen,
	    dropWhen : Elm.Signal.dropWhen,
	    dropRepeats : Elm.Signal.dropRepeats,
	    sampleOn : Elm.Signal.sampleOn
	    };

}();

(function() {
  var include = function(library) {
    for (var i in library) {
	Elm.Prelude[i] = library[i];
    }
  };
  include (Elm.Graphics.Color);
  include (Elm.Graphics.Text);
  include (Elm.Graphics.Element);

  show = Value.show;
  
}());
try{if(Elm.Automaton)throw "Module name collision, 'Automaton' is already defined.";Elm.Automaton=function(){try{if(!(Elm.Prelude instanceof Object))throw 'module not found'}catch(e){throw "Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var hiddenVars=[];for(var i in Elm.Prelude){if(hiddenVars.indexOf(i)>=0)continue;this[i]=Elm.Prelude[i]};try{if(!(Elm.Data.List instanceof Object))throw 'module not found'}catch(e){throw "Module 'Data.List' is missing. Compile with --make flag or load missing module in a separate JavaScript file."};var unzip=Elm.Data.List.unzip
function Automaton_0(a1){return ["Automaton",a1]};var Listen_9=["Listen"],Ignore_10=["Ignore"]
function DragFrom_11(a1){return ["DragFrom",a1]};var count_8=init_6(0)(function(__75){return function(c_76){return(1+c_76)}})
function run_1(Automaton$m0_15){return function(input_16){return function(){switch(Automaton$m0_15[0]){case"Automaton":return lift(fst)(foldp_(function(a_18){return function(Tuple2$bAutomaton$m_19){return function(){switch(Tuple2$bAutomaton$m_19[0]){case"Tuple2":switch(Tuple2$bAutomaton$m_19[2][0]){case"Automaton":return Tuple2$bAutomaton$m_19[2][1](a_18)};break};throw "Non-exhaustive pattern match in case"}()}})(Automaton$m0_15[1])(input_16))};throw "Non-exhaustive pattern match in case"}()}}
function step_2(a_22){return function(Automaton$m_23){return function(){switch(Automaton$m_23[0]){case"Automaton":return Automaton$m_23[1](a_22)};throw "Non-exhaustive pattern match in case"}()}}
function composeAuto_3(a1_25){return function(a2_26){return function(){var Automaton$m1_27=a1_25,m1_28=function(){switch(Automaton$m1_27[0]){case"Automaton":return Automaton$m1_27[1]};throw "Non-exhaustive pattern match in case"}(),Automaton$m2_29=a2_26,m2_30=function(){switch(Automaton$m2_29[0]){case"Automaton":return Automaton$m2_29[1]};throw "Non-exhaustive pattern match in case"}();return Automaton_0(function(a_33){return function(){var Tuple2$bm1__34=m1_28(a_33),b_35=function(){switch(Tuple2$bm1__34[0]){case"Tuple2":return Tuple2$bm1__34[1]};throw "Non-exhaustive pattern match in case"}(),m1__36=function(){switch(Tuple2$bm1__34[0]){case"Tuple2":return Tuple2$bm1__34[2]};throw "Non-exhaustive pattern match in case"}();return function(){var Tuple2$cm2__41=m2_30(b_35),c_42=function(){switch(Tuple2$cm2__41[0]){case"Tuple2":return Tuple2$cm2__41[1]};throw "Non-exhaustive pattern match in case"}(),m2__43=function(){switch(Tuple2$cm2__41[0]){case"Tuple2":return Tuple2$cm2__41[2]};throw "Non-exhaustive pattern match in case"}();return ["Tuple2",c_42,composeAuto_3(m1__36)(m2__43)]}()}()})}()}}
function combine_4(autos_48){return Automaton_0(function(a_49){return function(){var Tuple2$bsautos__50=unzip(map(function(Automaton$m_53){return function(){switch(Automaton$m_53[0]){case"Automaton":return Automaton$m_53[1](a_49)};throw "Non-exhaustive pattern match in case"}()})(autos_48)),bs_51=function(){switch(Tuple2$bsautos__50[0]){case"Tuple2":return Tuple2$bsautos__50[1]};throw "Non-exhaustive pattern match in case"}(),autos__52=function(){switch(Tuple2$bsautos__50[0]){case"Tuple2":return Tuple2$bsautos__50[2]};throw "Non-exhaustive pattern match in case"}();return ["Tuple2",bs_51,combine_4(autos__52)]}()})}
function pure_5(f_59){return Automaton_0(function(x_60){return ["Tuple2",f_59(x_60),pure_5(f_59)]})}
function init_6(s_61){return function(step_62){return Automaton_0(function(a_63){return function(){var s__64=step_62(a_63)(s_61);return ["Tuple2",s__64,init_6(s__64)(step_62)]}()})}}
function init__7(s_65){return function(step_66){return Automaton_0(function(a_67){return function(){var Tuple2$bs__68=step_66(a_67)(s_65),b_69=function(){switch(Tuple2$bs__68[0]){case"Tuple2":return Tuple2$bs__68[1]};throw "Non-exhaustive pattern match in case"}(),s__70=function(){switch(Tuple2$bs__68[0]){case"Tuple2":return Tuple2$bs__68[2]};throw "Non-exhaustive pattern match in case"}();return ["Tuple2",b_69,init__7(s__70)(step_66)]}()})}}
function vecSub_12(Tuple2$x1y1_77){return function(Tuple2$x2y2_78){return function(){switch(Tuple2$x1y1_77[0]){case"Tuple2":return function(){switch(Tuple2$x2y2_78[0]){case"Tuple2":return ["Tuple2",(Tuple2$x1y1_77[1]-Tuple2$x2y2_78[1]),(Tuple2$x1y1_77[2]-Tuple2$x2y2_78[2])]};throw "Non-exhaustive pattern match in case"}()};throw "Non-exhaustive pattern match in case"}()}}
function stepDrag_13(Tuple2$presspos_83){return function(Tuple2$dsform_84){return function(){switch(Tuple2$presspos_83[0]){case"Tuple2":return function(){switch(Tuple2$dsform_84[0]){case"Tuple2":return function(){function wrap_89(ds__90){return ["Tuple2",Tuple2$dsform_84[2],["Tuple2",ds__90,Tuple2$dsform_84[2]]]};return function(){switch(Tuple2$dsform_84[1][0]){case"DragFrom":return(Tuple2$presspos_83[1]?["Tuple2",uncurry(move)(vecSub_12(Tuple2$presspos_83[2])(Tuple2$dsform_84[1][1]))(Tuple2$dsform_84[2]),["Tuple2",DragFrom_11(Tuple2$dsform_84[1][1]),Tuple2$dsform_84[2]]]:function(){var form__92=uncurry(move)(vecSub_12(Tuple2$presspos_83[2])(Tuple2$dsform_84[1][1]))(Tuple2$dsform_84[2]);return ["Tuple2",form__92,["Tuple2",Listen_9,form__92]]}());case"Ignore":return wrap_89((Tuple2$presspos_83[1]?Ignore_10:Listen_9));case"Listen":return wrap_89((not(Tuple2$presspos_83[1])?Listen_9:(isWithin(Tuple2$presspos_83[2])(Tuple2$dsform_84[2])?DragFrom_11(Tuple2$presspos_83[2]):Ignore_10)))};throw "Non-exhaustive pattern match in case"}()}()};throw "Non-exhaustive pattern match in case"}()};throw "Non-exhaustive pattern match in case"}()}}
function dragForm_14(form_93){return init__7(["Tuple2",Listen_9,form_93])(stepDrag_13)};return {Automaton:Automaton_0,run:run_1,step:step_2,composeAuto:composeAuto_3,combine:combine_4,pure:pure_5,init:init_6,init_:init__7,count:count_8,Listen:Listen_9,Ignore:Ignore_10,DragFrom:DragFrom_11,vecSub:vecSub_12,stepDrag:stepDrag_13,dragForm:dragForm_14}}();Elm.main=function(){return Elm.Automaton.main}}catch(e){Elm.main=function(){var msg=('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>'+'<br/><span style="color:grey">Runtime Error in Automaton module:<br/>'+e+'<br/><br/>The problem may stem from an improper usage of:<br/>fst, unzip</span>');document.body.innerHTML=Text.monospace(msg);throw e}}