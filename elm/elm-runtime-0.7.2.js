
Elm = {};

var Guid = function() {
 var counter = 0;
 var guid = function() { counter += 1; return counter; };
 return {guid : guid};
}();
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
		w,h,1,["Nothing"],["Nothing"]];
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

var JSjson = window.JSON;
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
        return (k in obj[1]) ? ["Just", obj[1][k]] : ["Nothing"] ;
      };
    }
    function find(tipe,base) { return function (key) { return function(obj) {
          var k = JS.castStringToJSString(key);
          if (k in obj[1]) {
	    var v = obj[1][k];
	    if (v[0] === tipe) { return v[1]; }
          }
          return base;
        };
      }
    }
    function lookupWithDefault(base) { return function(key) { return function(obj) {
          var k = JS.castStringToJSString(key);
          return (k in obj[1]) ? obj[1][k] : base ;
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
        var obj;
	try {
	    obj = JSjson.parse(str);
	} catch (e) {
            return Elm.Maybe.Nothing;
	}
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
	return Elm.Maybe.Just( ['JSON',obj] );
    }
    return {empty : empty,
	    singleton : singleton,
	    insert : insert,
	    lookup : lookup,
	    findNumber : find("JsonNumber",0),
	    findString : find("JsonString",["Nil"]),
	    findObject : find("JsonObject", empty ),
	    findArray  : find("JsonArray" ,["Nil"]),
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
	if (x !== null && '_' in x) {
	    for (var i in x) { if (x[i] != y[i]) return false; }
	    for (var i in y) { if (!(i in x)) return false; }
	    return true;
	}
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
    arr = arr.join('');
    if (arr[arr.length-1] === " ") {
	return arr.slice(0,-1) + '&nbsp;';
    }
    return arr;
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
    var cStyle = window.getComputedStyle(t,null);
    var realW = cStyle.getPropertyValue("width").slice(0,-2) - 0;
    var realH = cStyle.getPropertyValue("height").slice(0,-2) - 0;
    document.body.removeChild(t);
    //delete t;
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
    //delete t;
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
    var cStyle = window.getComputedStyle(t,null);
    var w = cStyle.getPropertyValue("width").slice(0,-2) - 0;
    var h = cStyle.getPropertyValue("height").slice(0,-2) - 0;
    document.body.removeChild(t);
    //delete t;
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
    if (typeof v === "function") {
	return "<function>";
    } else if (typeof v === "boolean") {
	return v ? "True" : "False";
    } else if (typeof v === "number") {
	return v+"";
    } else if (typeof v === "string" && v.length < 2) {
	return "'"+v+"'";
    } else if (typeof v === "object" && ('_' in v)) {
	var output = [];
	for (var k in v._) {
	  console.log(k,v._[k]);
          for (var i = v._[k].length; i--; ) {
            output.push(k + " = " + toString(v._[k][i]));
	  }
	}
	for (var k in v) {
          if (k === '_') continue;
	  output.push(k + " = " + toString(v[k]));
	}
	if (output.length === 0) return "{}";
	return "{ " + output.join(", ") + " }";
    } else if (v[0]) {
	if (v[0].substring(0,5) === "Tuple") {
	    var output = new Array(v.length-1);
	    for (var i = v.length; --i; ) { output[i-1] = toString(v[i]); }
	    return "(" + output.join(",") + ")";
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
	    return "(JSON.fromList " + toString(Elm.JSON.toList(v)) + ")";
	} else if (v[0] === "RBNode" || v[0] === "RBEmpty") {
	    function cons(k){ return function(v) { return function(acc) { return ["Cons",["Tuple2",k,v],acc]; }; }; }
	    var list = Elm.Dict.foldr(cons)(["Nil"])(v);
	    var name = "Dict";
	    if (list[0] === "Cons" && list[1][2][0] === "Tuple0") {
		name = "Set";
		list = Elm.List.map(function(x) { return x[1]; })(list);
	    }
	    return "(" + name + ".fromList " + toString(list) + ")";
	} else {
	    var output = "";
	    for (var i = v.length; --i; ) { output = ' ' + toString(v[i]) + output; }
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
	      p[0], p[1], 1, ["Nothing"], ["Nothing"]];
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
}();Elm.List = function() {

    var throwError = function(f) {
	throw new Error("Function '" + f + "' expecting a list!");
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
	    throw new Error("Error: 'head' only accepts lists of length greater than one.");
	}
	return v[1];
    }
    function tail(v) {
	if (v[0] !== "Cons") {
	    throw new Error("Error: 'tail' only accepts lists of length greater than one.");
	}
	return v[2];
    }
    function last(v) {
	if (v[0] !== "Cons") {
	    throw new Error("Error: 'last' only accepts lists of length greater than one.");
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
	if (xs[0] === "Nil") { throw new Error("'foldr1' requires an non-empty list.") }
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
	    throw new Error("Error: 'scanl1' requires a list of at least length 1.");
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
}();/*! Maybe
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
/*! Either
!*/


Elm.Either = function() {
 /*[Definition]*/

 /** data Either a b = Left a | Right b
     Represents any data that can take two different types.

     This can also be used for error handling (`Either String a`) where
     error messages are stored on the left, and the correct values
     ("right" values) are stored on the right.
 **/
 function Left(a1) { return ['Left',a1]; }
 function Right(a1){ return ['Right',a1]; }

 /*[Basics]*/

 /** either : (a -> c) -> (b -> c) -> Either a b -> c
     Apply the first function to a `Left` and the second function to a `Right`.
     This allows the extraction of a value from an `Either`.
 **/
 function either(f){ return function(g){ return function(e){
    switch(e[0]){
     case 'Left':  return f(e[1]);
     case 'Right': return g(e[1]);
    }
 };};}

 /** isLeft : Either a b -> Bool
     True if the value is a `Left`.
 **/
 function isLeft(e)  { return e[0] == 'Left';  }

 /** isRight : Either a b -> Bool
     True if the value is a `Right`.
 **/
 function isRight(e) { return e[0] == 'Right'; }

 /*[With Lists]*/

 function get(es) { return Elm.List.map(function(x){return x[1];})(es); }

 /** lefts : [Either a b] -> [a]
     Keep only the values held in `Left` values.
 **/
 function lefts(es) { return get(Elm.List.filter(isLeft)(es)); }

 /** rights : [Either a b] -> [a]
     Keep only the values held in `Right` values.
 **/
 function rights(es) { return get(Elm.List.filter(isRight)(es)); }

 /** partition : [Either a b] -> ([a],[b])
     Split into two lists, lefts on the left and rights on the right.
     So we have the equivalence:

         partition es == (lefts es, rights es)
 **/
 function partition(es) {
     var lrs = Elm.List.partition(isLeft)(es);
     lrs[1] = get(lrs[1]);
     lrs[2] = get(lrs[2]);
     return lrs;
 }
 return {Left:Left,
	 Right:Right,
	 either:either,
	 isLeft:isLeft,
	 isRight:isRight,
	 lefts:lefts,
	 rights:rights,
	 partition:partition};
}();
/*! Char !*/

/*[Classification]*/

/** isUpper : Char -> Bool
    Selects upper case letters.
**/
/** isLower : Char -> Bool
    Selects lower case letters.
**/
/** isDigit : Char -> Bool
    Selects ASCII digits (0..9).
**/
/** isOctDigit : Char -> Bool
    Selects ASCII octal digits (0..7).
**/
/** isHexDigit : Char -> Bool
    Selects ASCII hexadecimal digits (0..9a..fA..F).
**/

/*[Conversion]*/

/** toUpper : Char -> Char
    Convert to upper case.
**/
/** toLower : Char -> Char
    Convert to lower case.
**/
/** toLocaleUpper : Char -> Char
    Convert to upper case, according to any locale-specific case mappings.
**/
/** toLocaleLower : Char -> Char
    Convert to lower case, according to any locale-specific case mappings.
**/
/** toCode : Char -> Int
    Convert to unicode.
**/
/** fromCode : Int -> Char
    Convert from unicode.
**/

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
Elm.Color = function() {

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
function complement(rgb) {
  var hsv = toHSV(rgb);
  hsv.hue = (hsv.hue + 180) % 360;
  return toRGB(hsv);
}

function hsva(h) { return function(s) { return function(v) { return function(a) {
    var clr = toRGB({hue:h, saturation:s, value:v});
    clr[4] = a;
    return clr;
  }; }; };
}

function hsv(h) { return function(s) { return function(v) {
   return toRGB({hue:h, saturation:s, value:v}); }; }; }

function toHSV(rgb) {
    var hsv = {};
    var r = rgb[1] / 255.0, g = rgb[2] / 255.0, b = rgb[3] / 255.0;
    var M = Math.max(r,g,b);
    var m = Math.min(r,g,b);
    var c = M - m;

    var h = 0;
    if (c === 0) { h = 0; }
    else if (M === r) { h = ((g - b) / c) % 6; }
    else if (M === g) { h = ((b - r) / c) + 2; }
    else if (M === b) { h = ((r - g) / c) + 4; }
    h *= 60;

    return { value : M, hue : h, saturation : (M === 0 ? 0 : c / M) };
}

function between(lo,hi,x) { return lo <= x && x < hi; }
function norm(n) { return Math.round(n*255); }

function toRGB(hsv) {
    var c = hsv.value * hsv.saturation;
    var hue = hsv.hue / 60;
    var x = c * (1 - Math.abs((hue % 2) - 1));
    var r = 0, g = 0, b = 0;
         if (between(0,1,hue)) { r = c; g = x; b = 0; }
    else if (between(1,2,hue)) { r = x; g = c; b = 0; }
    else if (between(2,3,hue)) { r = 0; g = c; b = x; }
    else if (between(3,4,hue)) { r = 0; g = x; b = c; }
    else if (between(4,5,hue)) { r = x; g = 0; b = c; }
    else if (between(5,6,hue)) { r = c; g = 0; b = x; }

    var m = hsv.value - c;
    return ["Color", norm(r+m), norm(g+m), norm(b+m), 1 ];
}
  return{rgba:rgba_1, rgb:rgb_2, hsva:hsva, hsv:hsv, red:red_3, green:green_4, blue:blue_5, yellow:yellow_6, cyan:cyan_7, magenta:magenta_8, black:black_9, white:white_10, gray:gray_11, grey:grey_12,complement:complement,extract:extract}
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
    ctx.strokeStyle = Elm.Color.extract(color);
    ctx.stroke();
};

function filled(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.fillStyle = Elm.Color.extract(color);
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
    ctx.strokeStyle = Elm.Color.extract(color);
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
    w = ~~w;
    h = ~~h;
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.width  = w;
    canvas.height = h;
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	function redo() { renderForms(this,ctx,w,h,forms); }
	renderForms(redo,ctx,w,h,forms);
	return canvas;
    }
    canvas.innerHTML = "Your browser does not support the canvas element.";
    return canvas;
};

function applyTransforms(theta,scale,x,y,w,h,e) {
  var t = "translate(" + (x - w / 2) + "px,"+ (y - h / 2) + "px)";
  var r = theta === (~~theta) ? "" : "rotate(" + theta*360 + "deg)";
  var s = scale === 1 ? "" : "scale(" + scale + "," + scale + ")";
  var transforms = t + " " + s + " " + r;
  e.style.transform       = transforms;
  e.style.msTransform     = transforms;
  e.style.MozTransform    = transforms;
  e.style.webkitTransform = transforms;
  e.style.OTransform      = transforms;
}

function collageElement(w,h,theta,scale,x,y,elem) {
    var e = Render.render(elem);
    applyTransforms(theta,scale,x,y,elem[3],elem[4],e);
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
  node.style.width = (~~w) + "px";
  node.style.height = (~~h) + "px";
  var f = nextSet;
  var next = nextSet[4][1];
  Render.update(node.firstChild, currSet[4][1], next);
  applyTransforms(f[1],f[2],f[3][1],f[3][2],next[3],next[4],node.firstChild);
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

}();Elm.Graphics = function() {
  for (this['i'] in Elm.List) {
      eval('var ' + this['i'] + ' = Elm.List[this.i];');
  }
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
  function basicNewElement(e,w,h) { return Element_37(Guid.guid(),e,w,h,1,["Nothing"],["Nothing"]); }
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
	return newElement_38(e[2], e[3], e[4], e[5], ["Just",c], e[7]);
    }
  }
  function link(lnk) {
    return function(e) {
	return newElement_38(e[2], e[3], e[4], e[5], e[6], ["Just", JS.castStringToJSString(lnk)]);
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
    var txt = Elm.Text.monospace(Value.toText(Value.show(v)));
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
	return basicNewElement(EFlow_43(DDown_3)(["Cons", e1_156, ["Cons", e2_157, ["Nil"]]]), Math.max(widthOf_52(e1_156),widthOf_52(e2_157)), heightOf_53(e1_156) + heightOf_53(e2_157));
    }
  }
  function below_62(e1_158) {
    return function(e2_159) {
	return basicNewElement(EFlow_43(DDown_3)(["Cons", e2_159, ["Cons", e1_158, ["Nil"]]]), Math.max(widthOf_52(e1_158),widthOf_52(e2_159)), heightOf_53(e1_158) + heightOf_53(e2_159));
    }
  }
  function beside_63(e1_160) {
    return function(e2_161) {
	return basicNewElement(EFlow_43(DRight_1)(["Cons", e1_160, ["Cons", e2_161, ["Nil"]]]), widthOf_52(e1_160) + widthOf_52(e2_161), Math.max(heightOf_53(e1_160),heightOf_53(e2_161)));
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
		return["Tuple2", w_177 / 2 * Math.cos(2 * (Math.PI / n_180) * i_182), h_178 / 2 * Math.sin(2 * (Math.PI / n_180) * i_182)];
            }
            return Shape_78(map(f_181)(function() {
              var lo = 0;
              var hi = n_180 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  lst = ["Cons", hi, lst]
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
          var m_187 = n_184;
          return function() {
            function f_188(i_189) {
		return["Tuple2", r_185 * Math.cos(2 * (Math.PI / m_187) * i_189), r_185 * Math.sin(2 * (Math.PI / m_187) * i_189)];
            }
            return Shape_78(map(f_188)(function() {
              var lo = 0;
              var hi = n_184 - 1;
              var lst = ["Nil"];
              if(lo <= hi) {
                do {
                  lst = ["Cons", hi, lst]
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
        throw new Error("Non-exhaustive pattern match in case");
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
          throw new Error("Non-exhaustive pattern match in case");
        }(Form$thetascaleTuple2$pxpyform_226)
      }
    }
  }
  return{left:left_6, right:right_7, down:down_8, up:up_9, inward:inward_10, outward:outward_11, topLeft:topLeft_22, topRight:topRight_23, bottomLeft:bottomLeft_24, bottomRight:bottomRight_25, midLeft:midLeft_26, midRight:midRight_27, midTop:midTop_28, midBottom:midBottom_29, middle:middle_30, middleAt:middleAt, topLeftAt:topLeftAt_31, topRightAt:topRightAt_32, bottomLeftAt:bottomLeftAt_33, bottomRightAt:bottomRightAt_34, absolute:absolute_35, relative:relative_36, width:width_47, height:height_48, size:size_49, opacity:opacity_50, 
	 color:color_51, link:link, widthOf:widthOf_52, heightOf:heightOf_53, sizeOf:sizeOf_54, text:text_56, asText:asText, plainText:plainText, centeredText:centeredText, justifiedText:justifiedText, rightedText:rightedText, image:image_57, images:images, video:video_58, fittedImage:fittedImage_59, flow:flow_60, above:above_61, below:below_62, beside:beside_63, layers:layers_64, collage:collage_65, spacer:spacer_66, container:container_67, line:line_76, segment:segment_77, polygon:polygon_79, rect:rect_80, oval:oval_81, circle:circle_82, ngon:ngon_83, solid:solid_89, dotted:dotted_90, dashed:dashed_91, customLine:customLine_92, filled:filled_93, 
	 outlined:outlined_94, customOutline:customOutline_95, textured:textured, sprite:sprite_96, toForm:toForm_97, rotate:rotate_98, scale:scale_99, move:move_100,
	 isWithin: Collage.insideForm}
}();
Elm.Text = function() {
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
    return addStyle('color', Elm.Color.extract(c));
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
    var e = newElement('div');
    e.style.width  = w + 'px';
    e.style.height = h + 'px';
    e.style.position = "relative";
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
	img.style.position = 'absolute';
	img.style.margin = 'auto';

	var sw = w, sh = h;
	if (w / h > this.width / this.height) {
	    sh = Math.round(this.height * w / this.width);
	} else {
	    sw = Math.round(this.width * h / this.height);
	}
	img.style.width = sw + 'px';
	img.style.height = sh + 'px';
	img.style.left = ((w - sw) / 2) + 'px';
	img.style.top = ((h - sh) / 2) + 'px';
    };
    img.src = src;
    img.name = src;
    addTo(e,img);
    return e;
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
      var shift = "translate(" + ~~(-e.style.width.slice(0,-2) / 2) + "px," + ~~(-e.style.height.slice(0,-2) / 2) + "px)";
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
	e.style.backgroundColor = Elm.Color.extract(elem[6][1]);
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
	var goDir = function(x) { return x; };
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
	var clr = Elm.Color.extract(next[6][1]);
	if (clr !== node.style.backgroundColor) node.style.backgroundColor = clr;
    }
    if (next[7].length === 2) {
	if (curr[7].length === 1 || next[7][1] !== curr[7][1]) node.parentNode.href = next[7][1];
    }
    next[1] = curr[1];
}

return {render:render,update:update,addTo:addTo,newElement:newElement,flowWith:flowWith,goIn:goIn};

}(); 
Elm.Signal = function() {
  var send = function(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  };
  function input(base) {
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
  function lift(func,args) {
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
  function fold(func,base,baseIsFunc,input) {
    this.id = Guid.guid();
    this.value = baseIsFunc ? base(input.value) : base;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      if (changed) { this.value = func(input.value)(this.value); }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  };

  function dropIf(pred,base,input) {
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

  function dropRepeats(input) {
      this.id = Guid.guid();
      this.value = input.value;
      this.kids = [];
      this.recv = function(timestep, changed, parentID) {
	  var chng = changed && !Value.eq(this.value,input.value);
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

  function timeIndex(pair,s) {
      this.id = Guid.guid();
      var t = (new window.Date).getTime();
      this.value = pair ? Value.Tuple(t,s.value) : t;
      this.kids = [];
      this.recv = function(timestep, changed, parentID) {
	  if (changed) this.value = pair ? Value.Tuple(timestep, s.value) : timestep;
	  send(this, timestep, changed);
      };
      s.kids.push(this);
  }

  function sampleOn(s1,s2) {
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

  function delay(t) { return function(s) {
      var delayed = new input(s.value);
      var firstEvent = true;
      function update(v) {
	  if (firstEvent) return;
	  setTimeout(function() { Dispatcher.notify(delayed.id,v); },t);
      }
      function first(a) { return function(b) { return a; }; }
      var out = new sampleOn(delayed,new lift(first, [delayed, new lift(update,[s])]));
      firstEvent = false;
      return out;
    };
  }
  
  function merge(s1,s2) {
    this.id = Guid.guid();
    this.value = s1.value;
    this.kids = [];
    this.next = null;
    this.count = 0;
    this.changed = false;
    
    this.recv = function(timestep, changed, parentID) {
      this.count += 1;
      if (changed) {
	  this.changed = true;
	  if (parentID == s2.id && this.next === null) { this.next = s2.value; }
	  if (parentID == s1.id) { this.next = s1.value; }
      }

      if (this.count == 2) {
	  if (this.changed) {
	      this.value = this.next;
	      this.next = null;
	  }
	  send(this, timestep, this.changed);
	  this.changed = false;
	  this.count = 0;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  };

  function merges(ss) {
      function mrg(x) { return function(y) { return new merge(x,y); }; }
      return Elm.List.foldl1(mrg)(ss);
  }

  function mergeEither(s1) { return function(s2) {
      function f(s) { return function(x) { return [s,x]; }; }
      return new merge(new lift(f('Left'),[s1]), new lift(f('Right'),[s2]));
    };
  }

  function average(sampleSize) { return function(s) {
    var sample = new Array(sampleSize);
    var i = sampleSize;
    while (i--) sample[i] = 0;
    i = 0;
    var full = false;
    var total = 0;
    function f(n) {
	total += n - sample[i];
	sample[i] = n;
	var avg = total / Math.max(1, full ? sampleSize : i);
	if (++i == sampleSize) { full = true; i = 0; }
	return avg;
    }
    return new lift(f, [s]);
   };
  }

  return {
    constant : function(v) { return new input(v); },
    lift  : function(f){return function(e){return new lift(f,[e]);};},
    lift2 : function(f) {
	      return function(e1) { return function(e2) {
	      return new lift(f, [e1,e2]); };};},
    lift3 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3){return new lift(f,[e1,e2,e3]);};};};},
    lift4 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3) { return function(e4) {
	      return new lift(f, [e1,e2,e3,e4]);};};};};},
    lift5 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3) { return function(e4) {
	      return function(e5) {
	      return new lift(f, [e1,e2,e3,e4,e5]);};};};};};},
    lift6 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
	      return new lift(f, [e1,e2,e3,e4,e5,e6]); };};};};};};},
    lift7 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
              return function(e7) {
              return new lift(f, [e1,e2,e3,e4,e5,e6,e7]);};};};};};};};},
    lift8 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
              return function(e7) { return function(e8) {
              return new lift(f, [e1,e2,e3,e4,e5,e6,e7,e8]);};};};};};};};};},
    foldp : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,false,e); }; }; },
    foldp$ : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,true,e); }; }; },
    foldp1 : function(f) { return function(e) {
	      return new fold(f,function(x){return x;},true,e); }; },
    delay : delay,
    merge : function(s1) { return function(s2) { return new merge(s1,s2)}; },
    merges : merges,
    mergeEither : mergeEither,
    average : average,
    count : function(sig) {
	  var incr = function(_){return function(c){return c+1;};};
	  return new fold(incr,0,false,sig) },
    countIf : function(pred) { return function(sig) {
	      var incr = function(x){return function(c){return pred(x) ? c+1 : c;};};
	      return new fold(incr,0,false,sig) }; },
    keepIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(function(x) { return !pred(x)},base,sig); }; }; },
    dropIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(pred,base,sig); }; }; },
    keepWhen : function(s) { return dropWhen(new lift(function(b){return !b;},[s])); },
    dropWhen : dropWhen,
    dropRepeats : function(s) { return new dropRepeats(s);},
    sampleOn : function(s1){return function(s2){return new sampleOn(s1,s2);};},
    timestamp : function(s) { return new timeIndex(true, s); },
    timeOf : function(s) { return new timeIndex(false, s); }
  };
}();
var Dispatcher = function() {
    var program = null;
    var inputs = [];
    var currentElement = null;

    var initialize = function() {
	program = Elm.main();
	if (!('recv' in program)) {
	    program = Elm.Signal.constant(program);
	}

	currentElement = program.value;
	filterDeadInputs();

	var content = document.getElementById('content');
	content.appendChild(Render.render(currentElement));
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Elm.Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
	program = Elm.Signal.lift(function(value) {
		var content = document.getElementById('content');
		Render.update(content.firstChild,currentElement,value);
		currentElement = value;
		return value;
	    })(program);
    };
    var notify = function(id, v) {
	var timestep = (new window.Date).getTime();
	var hasListener = false;
	for (var i = inputs.length; i--; ) {
	    hasListener = inputs[i].recv(timestep, id, v) || hasListener;
	}
	return hasListener;
    };

    function isAlive(input) {
	if (!('defaultNumberOfKids' in input)) return true;
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
}();
/*! HTTP
A library for asynchronous HTTP requests (AJAX). See the
[WebSocket](http://elm-lang.org/docs/WebSocket.elm) library if
you have very strict latency requirements.
!*/

Elm.HTTP = function() {
  var JS = Elm.JavaScript;
  var toElmString = Elm.JavaScript.castJSStringToString;

  /*[Creating Requests]*/

  /** get : String -> Request String
      Create a GET request to the given url.
  **/
  function get(url) { return request("GET")(url)(null)(["Nil"]); }

  /** post : String -> String -> Request String
      Create a POST request to the given url, carrying the given data.
  **/
  function post(url) { return function(data) {
	  return request("POST")(url)(data)(["Nil"]); }; }

  /** request : String -> String -> String -> [(String,String)] -> Request String
      Create a customized request. Arguments are request type (get, post, put,
      delete, etc.), target url, data, and a list of additional headers.
  **/
  function request(verb) { return function(url) { return function(data) {
    return function(headers) {
	return {0 : "Request",
		length : 1,
		verb : JS.castStringToJSString(verb),
		url : JS.castStringToJSString(url),
		data : data === null ? null : JS.castStringToJSString(data),
		headers : headers }; }; }; };
  }

  function registerReq(queue,responses) { return function(req) {
    if (req.url !== "") { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
      if (queue.length > 0) {
	Dispatcher.notify(responses.id, queue[0].value);
        if (queue[0].value[0] !== "Waiting") {
	  queue.shift();
	  setTimeout(function() { updateQueue(queue,responses); }, 0);
	}
      }
  }

  function sendReq(queue,responses,req) {
    var response = { value: ["Waiting"] };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
	if (request.readyState === 4) {
	    response.value = (request.status === 200
			      ? ["Success", toElmString(request.responseText)]
			      : ["Failure", request.status,
				 toElmString(request.statusText)]);
	    setTimeout(function() { updateQueue(queue,responses); }, 0);
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
  }

  /*[Responses]*/

  /** data Response a = Waiting | Success a | Failure Int String
      The datatype for responses. Success contains only the returned message.
      Failures contain both an error code and an error message.
  **/

  /*[Sending Requests]*/

  /** send : Signal (Request a) -> Signal (Response String)
      Performs an HTTP request with the given requests. Produces a signal
      that carries the responses.
  **/
  function send(requests) {
    var responses = Elm.Signal.constant(["Waiting"]);
    var sender = Elm.Signal.lift(registerReq([],responses))(requests);
    function f(x) { return function(y) { return x; } }
    return Elm.Signal.lift2(f)(responses)(sender);
  }

  /** sendGet : Signal String -> Signal (Response String)
      Performs an HTTP GET request with the given urls. Produces a signal
      that carries the responses.
  **/

  return {get   : get,
	  post  : post,
	  request : request,
	  send  : send,
	  sendGet : function(urls){return send(Elm.Signal.lift(get)(urls));}
	  };
}();
/*! WebSocket
A library for low latency HTTP communication. See the HTTP library for standard
requests like GET, POST, etc.
!*/

Elm.WebSocket = function() {
  var JS = Elm.JavaScript;

  /** open : String -> Signal String -> Signal String
      Create a web-socket. The first argument is the URL of the desired
      web-socket server. The input signal holds the outgoing messages,
      and the resulting signal contains the incoming ones.
   **/
  function open(url) { return function(outgoing) {
      var incoming = Elm.Signal.constant(["Nil"]);
      var ws = new window.WebSocket(JS.castStringToJSString(url));

      var pending = [];
      var ready = false;

      ws.onopen = function(e) {
	  var len = pending.length;
	  for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
	  ready = true;
      };
      ws.onmessage = function(event) {
        Dispatcher.notify(incoming.id, JS.castJSStringToString(event.data));
      };

      function send(msg) {
	  var s = JS.castStringToJSString(msg);
	  ready ? ws.send(s) : pending.push(s);
      }

      function take1(x) { return function(y) { return x; } }
      return Elm.Signal.lift2(take1)(incoming)(Elm.Signal.lift(send)(outgoing));
    };
  }

  return {open:open};
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
	    var str = Value.toText(options[1][1]);
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

/*! Keyboard
These are nicely curated inputs from the keyboard. See the
[Keyboard.Raw library](/docs/Signal/KeyboardRaw.elm) for a
lower-level interface that will let you define more complicated behavior.
!*/

(function() {
  function keySignal(f) {
    var signal = Elm.Signal.lift(f)(Elm.Keyboard.Raw.keysDown);
    Elm.Keyboard.Raw.keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(left,right,up,down) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks[0] == "Cons") {
	switch (ks[1]) {
	case left : --x; break;
	case right: ++x; break;
	case up   : ++y; break;
	case down : --y; break;
	}
	ks = ks[2];
      }
      return { _:[true], x:[x], y:[y] };
    }
    return keySignal(f);
  }

  function is(key) {
    function f(ks) {
      while (ks[0] == "Cons") {
	if (key == ks[1]) return true;
	ks = ks[2];
      }
      return false;
    }
    return keySignal(f);
  }

  /*[Directions]*/

  /** arrows : Signal { x:Int, y:Int }
      A signal of records indicating which arrow keys are pressed.

      `{ x = 0, y = 0 }` when pressing no arrows.
      `{ x =-1, y = 0 }` when pressing the left arrow.
      `{ x = 1, y = 1 }` when pressing the up and right arrows.
      `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
  **/
  Elm.Keyboard.arrows = dir(37,39,38,40);

  /** wasd : Signal { x:Int, y:Int }
      Just like the arrows signal, but this uses keys w, a, s, and d,
      which are common controls for many computer games.
  **/
  Elm.Keyboard.wasd   = dir(65,68,87,83);

  /*[Modifiers]*/

  /** shift : Signal Bool
      Whether the shift key is pressed.
  **/
  Elm.Keyboard.shift  = is(16);

  /** ctrl : Signal Bool
      Whether the control key is pressed.
  **/
  Elm.Keyboard.ctrl   = is(17);

  /** space : Signal Bool
      Whether the space key is pressed.
  **/
  Elm.Keyboard.space  = is(32);

}());
/*! Mouse
  !*/

Elm.Mouse = function() {
  /*[Position]*/

  /** position : Signal (Int,Int)
      The current mouse position.
  **/
  var position  = Elm.Signal.constant(Value.Tuple(0,0));
  position.defaultNumberOfKids = 2;

  /** x : Signal Int
      The current x-coordinate of the mouse.
  **/
  var x = Elm.Signal.lift(function(p){return p[1];})(position);
  x.defaultNumberOfKids = 0;

  /** y : Signal Int
      The current y-coordinate of the mouse.
  **/
  var y = Elm.Signal.lift(function(p){return p[2];})(position);
  y.defaultNumberOfKids = 0;

  /*[Button Status]*/

  /** isDown : Signal Bool
      The current state of the left mouse-button.
      True when the button is down, and false otherwise.
   **/
  var isDown    = Elm.Signal.constant(false);

  /** isClicked : Signal Bool
      True immediately after the left mouse-button has been clicked,
      and false otherwise.
   **/
  var isClicked = Elm.Signal.constant(false);

  /** clicks : Signal ()
      Always equal to unit. Event triggers on every mouse click.
   **/
  var clicks = Elm.Signal.constant(Value.Tuple());
  
  function getXY(e) {
    var posx = 0;
    var posy = 0;
    if (!e) e = window.event;
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

  /** isClickedOn : Element -> (Element, Signal Bool)
      Determine whether an element has been clicked. The resulting pair
      is a signal of booleans that is true when its paired element has
      been clicked. The signal is True immediately after the left
      mouse-button has been clicked, and false otherwise.
   **/
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
  }();/*! Random
  !*/

Elm.Random = function() {
  /*[In a Range]*/

  /** inRange : Int -> Int -> Signal Int
      Given a range from low to high, this produces a random number
      between 'low' and 'high' inclusive. The value in the signal does
      not change after the page has loaded.
  **/
  var inRange = function(min) { return function(max) {
      return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
    };
  };

  /** randomize : Int -> Int -> Signal a -> Signal Int
      Given a range from low to high and a signal of values, this produces
      a new signal that changes whenever the input signal changes. The new
      values are random number between 'low' and 'high' inclusive.
  **/
  var randomize = function(min) { return function(max) { return function(signal) {
      function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
      return Elm.Signal.lift(f)(signal);
    };
   };
  };
  return { inRange:inRange, randomize:randomize };
}();/*! Time
Library for working with time. Type `Time` represents some number of
milliseconds.
!*/

Elm.Time = function() {

  /*[Times]*/

  /** hour, minute, second, ms : Time
      Units of time, making it easier to specify things like a
      half-second `(second / 2)`.
  **/

  function timeNow() { return (new window.Date).getTime(); }

  /*[Tickers]*/

  /** fps : Number -> Signal Time
      Takes desired number of frames per second (fps). The resulting signal
      gives a sequence of time deltas as quickly as possible until it reaches
      the desired FPS. A time delta is the time between the last frame and the
      current frame.
  **/

  /** fpsWhen : Number -> Signal Bool -> Signal Time
      Same as the fps function, but you can turn it on and off. Allows you
      to do brief animations based on user input without major ineffeciencies.
      The first time delta after a pause is always zero, no matter how long
      the pause was. This way summing the deltas will actually give the amount
      of time that the output signal has been running.
  **/
  function fpsWhen(desiredFPS) { return function (isOn) {
      var msPerFrame = 1000 / desiredFPS;
      var prev = timeNow(), curr = prev, diff = 0, wasOn = true;
      var ticker = Elm.Signal.constant(diff);
      function tick(zero) { return function() {
	  curr = timeNow();
	  diff = zero ? 0 : curr - prev;
	  prev = curr;
	  Dispatcher.notify(ticker.id, diff);
        };
      }
      var timeoutID = 0;
      function f(isOn) { return function(t) {
	if (isOn) {
	    timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
	} else if (wasOn) {
	    clearTimeout(timeoutID);	    
	}
	wasOn = isOn;
	return t;
       };
      }
      return Elm.Signal.lift2(f)(isOn)(ticker);
    };
  }
 
  /** every : Time -> Signal Time
      Takes a time interval t. The resulting signal is the current time,
      updated every t.
   **/
  function everyWhen(isOn) { return function(t) {
      var clock = Elm.Signal.constant(timeNow());
      function tellTime() { Dispatcher.notify(clock.id, timeNow()); }
      setInterval(tellTime, t);
      return clock;
    };
  }

  function since(t) { return function(s) {
	  function cmp(a) { return function(b) { return !Value.eq(a,b); }; }
	  var dcount = Elm.Signal.count(Elm.Signal.delay(t)(s));
	  return Elm.Signal.lift2(cmp)(Elm.Signal.count(s))(dcount);
      };
  }
  function after(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(false);
      setTimeout(function() { Dispatcher.notify(thread.id, true); }, t);
      return thread;
  }
  function before(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(true);
      setTimeout(function() { Dispatcher.notify(thread.id, false); }, t);
      return thread;
  }
  function read(s) {
      var t = window.Date.parse(s);
      return isNaN(t) ? ["Nothing"] : ["Just",t];
  }
  return {fpsWhen : fpsWhen,
	  fps : function(t) { return fpsWhen(t)(Elm.Signal.constant(true)); },
	  every : everyWhen(Elm.Signal.constant(true)),
	  delay : Elm.Signal.delay,
	  since : since,
	  after  : after,
	  before : before,
	  hour   : 3600000,
	  minute : 60000,
	  second : 1000,
	  ms     : 1,
	  inHours   : function(t) { return t / 3600000; },
	  inMinutes : function(t) { return t / 60000; },
	  inSeconds : function(t) { return t / 1000; },
	  inMss     : function(t) { return t; },
	  toDate : function(t) { return new window.Date(t); },
	  read   : read
  };

}();/*! Window !*/

Elm.Window = function() {

  /*[Dimensions]*/

  /** dimensions : Signal (Int,Int)
      The current dimensions of the window (i.e. the area viewable to the
      user, not including scroll bars).
  **/
  var dimensions = Elm.Signal.constant(Value.Tuple(window.innerWidth,
						   window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  /** width : Signal Int
      The current width of the window.
  **/
  var width  = Elm.Signal.lift(function(p){return p[1];})(dimensions);
  width.defaultNumberOfKids = 0;

  /** height : Signal Int
      The current height of the window.
  **/
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

Elm.Date = function() {

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(Elm.JavaScript.castStringToJSString(str));
     if (isNaN(d.getTime())) return ["Nothing"];
     return ["Just",d];
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 return {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return [monthTable[d.getMonth()]]; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     dayOfWeek : function(d) { return [dayTable[d.getDay()]]; },
     toTime  : function(d) { return d.getTime(); },
     Mon : ["Mon"], Tue : ["Tue"], Wed : ["Wed"],
     Thu : ["Thu"], Fri : ["Fri"], Sat : ["Sat"], Sun : ["Sun"],
     Jan : ["Jan"], Feb : ["Feb"], Mar : ["Mar"], Apr : ["Apr"],
     May : ["May"], Jun : ["Jun"], Jul : ["Jul"], Aug : ["Aug"],
     Sep : ["Sep"], Oct : ["Oct"], Nov : ["Nov"], Dec : ["Dec"]
 };

}();
function elmRecordCopy(r) {
    var o = {};
    for (var i in r) { o[i] = r[i]; }
    return o;
}

function elmRecordRemove(x,r) {
    var o = elmRecordCopy(r);
    if (x in o._) {
	o[x] = o._[x][0];
	o._[x] = o._[x].slice(1);
	if (o._[x].length === 0) { delete o._[x]; }
    } else {
	delete o[x];
    }
    return o;
}

function elmRecordReplace(kvs,r) {
    var o = elmRecordCopy(r);
    for (var i = kvs.length; i--; ) {
	kvsi = kvs[i];
	o[kvsi[0]] = kvsi[1];
    }
    return o;
}

function elmRecordInsert(x,v,r) {
    var o = elmRecordCopy(r);
    if (x in o) o._[x] = [o[x]].concat(x in o._ ? o._[x].slice(0) : []);
    o[x] = v;
    return o;
}

Value.addListener(document, 'elm_log', function(e) { console.log(e.value); });
Value.addListener(document, 'elm_title', function(e) {document.title = e.value;});
Value.addListener(document, 'elm_redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
    });
Value.addListener(document, 'elm_viewport', function(e) {
	var node = document.getElementById('elm_viewport');
	if (!node) {
	    node = document.createElement('meta');
	    node.id = 'elm_viewport';
	    node.name = 'viewport';
	    document.head.appendChild(node);
	}
	node.content = e.value;
	Dispatcher.notify(Elm.Window.dimensions.id,
			  Value.Tuple(window.innerWidth, window.innerHeight));
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
		throw new Error("Function was uncurry'd but was not given a pair.");
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
	    xor  : function(x) { return function(y) { return x != y; }; },
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
	    foldp$ : Elm.Signal.foldp$,
	    constant : Elm.Signal.constant,
	    merge : Elm.Signal.merge,
	    merges : Elm.Signal.merges,
	    mergeEither : Elm.Signal.mergeEither,
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
/*! Touch
This is an early version of the touch library. It will likely grow to
include gestures that would be useful for both games and web-pages.
!*/

Elm.Touch = function() {

  function Dict() {
    this.keys = [];
    this.values = [];

    this.insert = function(key,value) {
      this.keys.push(key);
      this.values.push(value);
    };
    this.lookup = function(key) {
      var i = this.keys.indexOf(key)
      return i >= 0 ? this.values[i] : {x:0,y:0,t:0};
    };
    this.remove = function(key) {
      var i = this.keys.indexOf(key);
      if (i < 0) return;
      var t = this.values[i];
      this.keys.splice(i,1);
      this.values.splice(i,1);
      return t;
    };
  }

  var root = Elm.Signal.constant([]),
      tapTime = 500,
      hasTap = false,
      tap = {_:[true],x:[0],y:[0]},
      dict = new Dict();

  function touch(t) {
      var r = dict.lookup(t.identifier);
      return {_ : [true], id: [t.identifier],
	      x: [t.pageX], y: [t.pageY],
	      x0: [r.x], y0: [r.y],
	      t0: [r.t] };
  }

  function start(e) {
    dict.insert(e.identifier,{x:e.pageX,y:e.pageY,t:Date.now()});
  }
  function end(e) {
    var t = dict.remove(e.identifier);
    if (Date.now() - t.t < tapTime) {
	hasTap = true;
	tap = {_:[true], x:[t.x], y:[t.y]};
    }
  }

  function listen(name, f) {
    function update(e) {
      for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
      var ts = new Array(e.touches.length);
      for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
      var hasListener = Dispatcher.notify(root.id, ts);
      if (!hasListener) return document.removeEventListener(name, update);
      e.preventDefault();
    }
    Value.addListener(document, name, update);
  }

  listen("touchstart", start);
  listen("touchmove", function(_){});
  listen("touchend", end);
  listen("touchcancel", end);
  listen("touchleave", end);

  function dependency(f) {
      var sig = Elm.Signal.lift(f)(root);
      root.defaultNumberOfKids += 1;
      sig.defaultNumberOfKids = 0;
      return sig;
  }

  /*[Touches]*/

  /** touches : Signal [{ x:Int, y:Int, id:Int, x0:Int, y0:Int, t0:Time }]
      A list of touches. Each ongoing touch is represented by a set of
      coordinates and an identifier id that allows you to distinguish
      between different touches. Each touch also contains the coordinates and
      time of the initial contact (x0, y0, and t0) which helps compute more
      complicated gestures.
  **/
  var touches = dependency(function(ts) {
	  return Elm.JavaScript.castJSArrayToList(ts);
      });

  /*[Gestures]*/

  /** taps : Signal { x:Int, y:Int }
      The last position that was tapped. Default value is `{x=0,y=0}`.
      Updates whenever the user taps the screen.
  **/
  var taps = function() {
      var sig = dependency(function(_) { return tap; });
      sig.defaultNumberOfKids = 1;
      function pred(_) { var b = hasTap; hasTap = false; return b; }
      var sig2 =  Elm.Signal.keepIf(pred)({_:[true],x:[0],y:[0]})(sig);
      sig2.defaultNumberOfKids = 0;
      return sig2;
  }();

  return { touches: touches, taps: taps };
}();
Elm.Dict=function(){
 var compare = Elm.Prelude.compare;
 var uncurry = Elm.Prelude.uncurry;
 var Nothing = Elm.Prelude.Nothing;
 var Just = Elm.Prelude.Just;
 var not = Elm.Prelude.not;
 var eq = Elm.Prelude.eq;
 var isJust=Elm.Maybe.isJust;
 var Red_0=['Red'];
 var Black_1=['Black'];
 function RBNode_2(a1){
  return function(a2){
   return function(a3){
    return function(a4){
     return function(a5){
      return ['RBNode',a1,a2,a3,a4,a5];};};};};}
 var RBEmpty_3=['RBEmpty'];
 function min_6(t_42){
  return function(){
  switch(t_42[0]){
   case 'RBEmpty':
   throw '(min RBEmpty) is not defined';
   case 'RBNode':
   switch(t_42[4][0]){
    case 'RBEmpty':
    return ['Tuple2',t_42[2],t_42[3]];
   }
   return min_6(t_42[4]);
  }
  throw new Error("Non-exhaustive pattern match in case");}();}
 function lookup_7(k_46){
  return function(t_47){
   return function(){
   switch(t_47[0]){
    case 'RBEmpty':
    return Nothing;
    case 'RBNode':
    return function(){
    var case12=compare(k_46)(t_47[2]);
    switch(case12[0]){
     case 'EQ':
     return Just(t_47[3]);
     case 'GT':
     return lookup_7(k_46)(t_47[5]);
     case 'LT':
     return lookup_7(k_46)(t_47[4]);
    }
    throw new Error("Non-exhaustive pattern match in case");}();
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function findWithDefault_8(base_52){
  return function(k_53){
   return function(t_54){
    return function(){
    switch(t_54[0]){
     case 'RBEmpty':
     return base_52;
     case 'RBNode':
     return function(){
     var case19=compare(k_53)(t_54[2]);
     switch(case19[0]){
      case 'EQ':
      return t_54[3];
      case 'GT':
      return findWithDefault_8(base_52)(k_53)(t_54[5]);
      case 'LT':
      return findWithDefault_8(base_52)(k_53)(t_54[4]);
     }
     throw new Error("Non-exhaustive pattern match in case");}();
    }
    throw new Error("Non-exhaustive pattern match in case");}();};};}
 function member_9(k_59){
  return function(t_60){
   return isJust(lookup_7(k_59)(t_60));};}
 function rotateLeft_10(t_61){
  return function(){
  switch(t_61[0]){
   case 'RBNode':
   switch(t_61[5][0]){
    case 'RBNode':
    return RBNode_2(t_61[1])(t_61[5][2])(t_61[5][3])(RBNode_2(Red_0)(t_61[2])(t_61[3])(t_61[4])(t_61[5][4]))(t_61[5][5]);
   }break;
  }
  throw 'rotateLeft of a node without enough children';}();}
 function rotateRight_11(t_71){
  return function(){
  switch(t_71[0]){
   case 'RBNode':
   switch(t_71[4][0]){
    case 'RBNode':
    return RBNode_2(t_71[1])(t_71[4][2])(t_71[4][3])(t_71[4][4])(RBNode_2(Red_0)(t_71[2])(t_71[3])(t_71[4][5])(t_71[5]));
   }break;
  }
  throw 'rotateRight of a node without enough children';}();}
 function rotateLeftIfNeeded_12(t_81){
  return function(){
  switch(t_81[0]){
   case 'RBNode':
   switch(t_81[5][0]){
    case 'RBNode':
    switch(t_81[5][1][0]){
     case 'Red':
     return rotateLeft_10(t_81);
    }break;
   }break;
  }
  return t_81;}();}
 function rotateRightIfNeeded_13(t_82){
  return function(){
  switch(t_82[0]){
   case 'RBNode':
   switch(t_82[4][0]){
    case 'RBNode':
    switch(t_82[4][1][0]){
     case 'Red':
     switch(t_82[4][4][0]){
      case 'RBNode':
      switch(t_82[4][4][1][0]){
       case 'Red':
       return rotateRight_11(t_82);
      }break;
     }break;
    }break;
   }break;
  }
  return t_82;}();}
 function otherColor_14(c_83){
  return function(){
  switch(c_83[0]){
   case 'Black':
   return Red_0;
   case 'Red':
   return Black_1;
  }
  throw new Error("Non-exhaustive pattern match in case");}();}
 function color_flip_15(t_84){
  return function(){
  switch(t_84[0]){
   case 'RBNode':
   switch(t_84[4][0]){
    case 'RBNode':
    switch(t_84[5][0]){
     case 'RBNode':
     return RBNode_2(otherColor_14(t_84[1]))(t_84[2])(t_84[3])(RBNode_2(otherColor_14(t_84[4][1]))(t_84[4][2])(t_84[4][3])(t_84[4][4])(t_84[4][5]))(RBNode_2(otherColor_14(t_84[5][1]))(t_84[5][2])(t_84[5][3])(t_84[5][4])(t_84[5][5]));
    }break;
   }break;
  }
  throw 'color_flip called on a RBEmpty or RBNode with a RBEmpty child';}();}
 function color_flipIfNeeded_16(t_98){
  return function(){
  switch(t_98[0]){
   case 'RBNode':
   switch(t_98[4][0]){
    case 'RBNode':
    switch(t_98[4][1][0]){
     case 'Red':
     switch(t_98[5][0]){
      case 'RBNode':
      switch(t_98[5][1][0]){
       case 'Red':
       return color_flip_15(t_98);
      }break;
     }break;
    }break;
   }break;
  }
  return t_98;}();}
 function fixUp_17(t_99){
  return color_flipIfNeeded_16(rotateRightIfNeeded_13(rotateLeftIfNeeded_12(t_99)));}
 function ensureBlackRoot_18(t_100){
  return function(){
  switch(t_100[0]){
   case 'RBNode':
   switch(t_100[1][0]){
    case 'Red':
    return RBNode_2(Black_1)(t_100[2])(t_100[3])(t_100[4])(t_100[5]);
   }break;
  }
  return t_100;}();}
 function insert_19(k_105){
  return function(v_106){
   return function(t_107){
    return function(){
     function ins_108(t_109){
      return function(){
      switch(t_109[0]){
       case 'RBEmpty':
       return RBNode_2(Red_0)(k_105)(v_106)(RBEmpty_3)(RBEmpty_3);
       case 'RBNode':
       return function(){
        var h_115=function(){
        var case114=compare(k_105)(t_109[2]);
        switch(case114[0]){
         case 'EQ':
         return RBNode_2(t_109[1])(t_109[2])(v_106)(t_109[4])(t_109[5]);
         case 'GT':
         return RBNode_2(t_109[1])(t_109[2])(t_109[3])(t_109[4])(ins_108(t_109[5]));
         case 'LT':
         return RBNode_2(t_109[1])(t_109[2])(t_109[3])(ins_108(t_109[4]))(t_109[5]);
        }
        throw new Error("Non-exhaustive pattern match in case");}();
        return fixUp_17(h_115);}();
      }
      throw new Error("Non-exhaustive pattern match in case");}();}
     return ensureBlackRoot_18(ins_108(t_107));}();};};}
 function singleton_20(k_116){
  return function(v_117){
   return insert_19(k_116)(v_117)(RBEmpty_3);};}
 function isRed_21(t_118){
  return function(){
  switch(t_118[0]){
   case 'RBNode':
   switch(t_118[1][0]){
    case 'Red':
    return true;
   }break;
  }
  return false;}();}
 function isRedLeft_22(t_119){
  return function(){
  switch(t_119[0]){
   case 'RBNode':
   switch(t_119[4][0]){
    case 'RBNode':
    switch(t_119[4][1][0]){
     case 'Red':
     return true;
    }break;
   }break;
  }
  return false;}();}
 function isRedLeftLeft_23(t_120){
  return function(){
  switch(t_120[0]){
   case 'RBNode':
   switch(t_120[4][0]){
    case 'RBNode':
    switch(t_120[4][4][0]){
     case 'RBNode':
     switch(t_120[4][4][1][0]){
      case 'Red':
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();}
 function isRedRight_24(t_121){
  return function(){
  switch(t_121[0]){
   case 'RBNode':
   switch(t_121[5][0]){
    case 'RBNode':
    switch(t_121[5][1][0]){
     case 'Red':
     return true;
    }break;
   }break;
  }
  return false;}();}
 function isRedRightLeft_25(t_122){
  return function(){
  switch(t_122[0]){
   case 'RBNode':
   switch(t_122[5][0]){
    case 'RBNode':
    switch(t_122[5][4][0]){
     case 'RBNode':
     switch(t_122[5][4][1][0]){
      case 'Red':
      return true;
     }break;
    }break;
   }break;
  }
  return false;}();}
 function moveRedLeft_26(t_123){
  return function(){
   var t__124=color_flip_15(t_123);
   return function(){
   switch(t__124[0]){
    case 'RBNode':
    return function(){
    switch(t__124[5][0]){
     case 'RBNode':
     switch(t__124[5][4][0]){
      case 'RBNode':
      switch(t__124[5][4][1][0]){
       case 'Red':
       return color_flip_15(rotateLeft_10(RBNode_2(t__124[1])(t__124[2])(t__124[3])(t__124[4])(rotateRight_11(t__124[5]))));
      }break;
     }break;
    }
    return t__124;}();
   }
   return t__124;}();}();}
 function moveRedRight_27(t_130){
  return function(){
   var t__131=color_flip_15(t_130);
   return (isRedLeftLeft_23(t__131)?color_flip_15(rotateRight_11(t__131)):t__131);}();}
 function moveRedLeftIfNeeded_28(t_132){
  return ((not(isRedLeft_22(t_132))&&not(isRedLeftLeft_23(t_132)))?moveRedLeft_26(t_132):t_132);}
 function moveRedRightIfNeeded_29(t_133){
  return ((not(isRedRight_24(t_133))&&not(isRedRightLeft_25(t_133)))?moveRedRight_27(t_133):t_133);}
 function deleteMin_30(t_134){
  return function(){
   function del_135(t_136){
    return function(){
    switch(t_136[0]){
     case 'RBNode':
     switch(t_136[4][0]){
      case 'RBEmpty':
      return RBEmpty_3;
     }break;
    }
    return function(){
    var case198=moveRedLeftIfNeeded_28(t_136);
    switch(case198[0]){
     case 'RBEmpty':
     return RBEmpty_3;
     case 'RBNode':
     return fixUp_17(RBNode_2(case198[1])(case198[2])(case198[3])(del_135(case198[4]))(case198[5]));
    }
    throw new Error("Non-exhaustive pattern match in case");}();}();}
   return ensureBlackRoot_18(del_135(t_134));}();}
 function remove_31(k_142){
  return function(t_143){
   return function(){
    function eq_and_noRightNode_144(t_150){
     return function(){
     switch(t_150[0]){
      case 'RBNode':
      switch(t_150[5][0]){
       case 'RBEmpty':
       return eq(k_142,t_150[2]);
      }break;
     }
     return false;}();}
    function eq_145(t_152){
     return function(){
     switch(t_152[0]){
      case 'RBNode':
      return eq(k_142,t_152[2]);
     }
     return false;}();}
    function delLT_146(t_154){
     return function(){
     var case216=moveRedLeftIfNeeded_28(t_154);
     switch(case216[0]){
      case 'RBEmpty':
      throw 'delLT on RBEmpty';
      case 'RBNode':
      return fixUp_17(RBNode_2(case216[1])(case216[2])(case216[3])(del_149(case216[4]))(case216[5]));
     }
     throw new Error("Non-exhaustive pattern match in case");}();}
    function delEQ_147(t_160){
     return function(){
     switch(t_160[0]){
      case 'RBEmpty':
      throw 'delEQ called on a RBEmpty';
      case 'RBNode':
      return function(){
       var Tuple2$k_v__164=min_6(t_160[5]);
       var k__165=function(){
       switch(Tuple2$k_v__164[0]){
        case 'Tuple2':
        return Tuple2$k_v__164[1];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       var v__166=function(){
       switch(Tuple2$k_v__164[0]){
        case 'Tuple2':
        return Tuple2$k_v__164[2];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       return fixUp_17(RBNode_2(t_160[1])(k__165)(v__166)(t_160[4])(deleteMin_30(t_160[5])));}();
     }
     throw new Error("Non-exhaustive pattern match in case");}();}
    function delGT_148(t_171){
     return function(){
     switch(t_171[0]){
      case 'RBEmpty':
      throw 'delGT called on a RBEmpty';
      case 'RBNode':
      return fixUp_17(RBNode_2(t_171[1])(t_171[2])(t_171[3])(t_171[4])(del_149(t_171[5])));
     }
     throw new Error("Non-exhaustive pattern match in case");}();}
    function del_149(t_177){
     return function(){
     switch(t_177[0]){
      case 'RBEmpty':
      return RBEmpty_3;
      case 'RBNode':
      return ((compare(k_142)(t_177[2])[0] === 'LT')?delLT_146(t_177):function(){
       var u_179=(isRedLeft_22(t_177)?rotateRight_11(t_177):t_177);
       return (eq_and_noRightNode_144(u_179)?u_179[4]:function(){
        var t__180=moveRedRightIfNeeded_29(t_177);
        return (eq_145(t__180)?delEQ_147(t__180):delGT_148(t__180));}());}());
     }
     throw new Error("Non-exhaustive pattern match in case");}();}
    return (member_9(k_142)(t_143)?ensureBlackRoot_18(del_149(t_143)):t_143);}();};}
 function map_32(f_181){
  return function(t_182){
   return function(){
   switch(t_182[0]){
    case 'RBEmpty':
    return RBEmpty_3;
    case 'RBNode':
    return RBNode_2(t_182[1])(t_182[2])(f_181(t_182[3]))(map_32(f_181)(t_182[4]))(map_32(f_181)(t_182[5]));
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function foldl_33(f_188){
  return function(acc_189){
   return function(t_190){
    return function(){
    switch(t_190[0]){
     case 'RBEmpty':
     return acc_189;
     case 'RBNode':
     return foldl_33(f_188)(f_188(t_190[2])(t_190[3])(foldl_33(f_188)(acc_189)(t_190[4])))(t_190[5]);
    }
    throw new Error("Non-exhaustive pattern match in case");}();};};}
 function foldr_34(f_195){
  return function(acc_196){
   return function(t_197){
    return function(){
    switch(t_197[0]){
     case 'RBEmpty':
     return acc_196;
     case 'RBNode':
     return foldr_34(f_195)(f_195(t_197[2])(t_197[3])(foldr_34(f_195)(acc_196)(t_197[5])))(t_197[4]);
    }
    throw new Error("Non-exhaustive pattern match in case");}();};};}
 function union_35(t1_202){
  return function(t2_203){
   return foldl_33(insert_19)(t2_203)(t1_202);};}
 function intersect_36(t1_204){
  return function(t2_205){
   return foldl_33(function(k_206){
    return function(v_207){
     return function(t_208){
      return (member_9(k_206)(t2_205)?insert_19(k_206)(v_207)(t_208):t_208);};};})(empty_4)(t1_204);};}
 function diff_37(t1_209){
  return function(t2_210){
   return foldl_33(function(k_211){
    return function(v_212){
     return function(t_213){
      return remove_31(k_211)(t_213);};};})(t1_209)(t2_210);};}
 function keys_38(t_214){
  return foldr_34(function(k_215){
   return function(v_216){
    return function(acc_217){
     return ['Cons',k_215,acc_217];};};})(['Nil'])(t_214);}
 function values_39(t_218){
  return foldr_34(function(k_219){
   return function(v_220){
    return function(acc_221){
     return ['Cons',v_220,acc_221];};};})(['Nil'])(t_218);}
 function toList_40(t_222){
  return foldr_34(function(k_223){
   return function(v_224){
    return function(acc_225){
     return ['Cons',['Tuple2',k_223,v_224],acc_225];};};})(['Nil'])(t_222);}
 function fromList_41(assocs_226){
  return Elm.List.foldl(uncurry(insert_19))(empty_4)(assocs_226);}
 var empty_4=RBEmpty_3;
 return {$op : {},
 empty:empty_4,
 lookup:lookup_7,
 findWithDefault:findWithDefault_8,
 member:member_9,
 insert:insert_19,
 singleton:singleton_20,
 remove:remove_31,
 map:map_32,
 foldl:foldl_33,
 foldr:foldr_34,
 union:union_35,
 intersect:intersect_36,
 diff:diff_37,
 keys:keys_38,
 values:values_39,
 toList:toList_40,
 fromList:fromList_41};}();

Elm.Set=function(){
 var empty_0=Elm.Dict.empty;
 var remove_3=Elm.Dict.remove;
 var member_4=Elm.Dict.member;
 var union_5=Elm.Dict.union;
 var intersect_6=Elm.Dict.intersect;
 var diff_7=Elm.Dict.diff;
 var toList_8=Elm.Dict.keys;
 var fromList_9=Elm.List.foldl(function(k_15){
  return function(t_16){
   return Elm.Dict.insert(k_15)(["Tuple0"])(t_16);};})(empty_0);
 function singleton_1(k_13){
  return Elm.Dict.singleton(k_13)(["Tuple0"]);};
 function insert_2(k_14){
  return Elm.Dict.insert(k_14)(["Tuple0"]);};
 function foldl_10(f_17){
  return Elm.Dict.foldl(function(k_18){
   return function(v_19){
    return function(b_20){
     return f_17(k_18)(b_20);};};});};
 function foldr_11(f_21){
  return Elm.Dict.foldr(function(k_22){
   return function(v_23){
    return function(b_24){
     return f_21(k_22)(b_24);};};});};
 function map_12(f_25){
  return function(t_26){
   return function(x){
    return fromList_9(Elm.List.map(f_25)(x));}(toList_8(t_26));};};
 return {empty:empty_0,singleton:singleton_1,insert:insert_2,remove:remove_3,member:member_4,union:union_5,intersect:intersect_6,diff:diff_7,toList:toList_8,fromList:fromList_9,foldl:foldl_10,foldr:foldr_11,map:map_12};}();

(function() {
try{

var $op={};
for(this['i'] in Elm){eval('var '+this['i']+'=Elm[this.i];');}
if (Elm.Automaton) throw new Error("Module name collision, 'Automaton' is already defined."); 
Elm.Automaton=function(){
 try{
  if (!(Elm.Prelude instanceof Object)) throw 'module not found';
 } catch(e) {
  throw ("Module 'Prelude' is missing. Compile with --make flag or load missing module in a separate JavaScript file.");
 }
 var hiddenVars={};
 for (this['i'] in Elm.Prelude) {
  if (hiddenVars[this['i']]) continue;
  eval('var ' + this['i'] + ' = Elm.Prelude[this.i];');}
 function Automaton_0(a1){
  return ["Automaton",a1];}
 var Listen_8=["Listen"];
 var Ignore_9=["Ignore"];
 function DragFrom_10(a1){
  return ["DragFrom",a1];}
 $op['>>>'] = function(a1_24){
  return function(a2_25){
   return function(){
    var Automaton$m1_26=a1_24;
    var m1_27=function(){
    switch(Automaton$m1_26[0]){
     case "Automaton":
     return Automaton$m1_26[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    var Automaton$m2_28=a2_25;
    var m2_29=function(){
    switch(Automaton$m2_28[0]){
     case "Automaton":
     return Automaton$m2_28[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    return Automaton_0(function(a_32){
     return function(){
      var Tuple2$bm1__33=m1_27(a_32);
      var b_34=function(){
      switch(Tuple2$bm1__33[0]){
       case "Tuple2":
       return Tuple2$bm1__33[1];
      }
      throw new Error("Non-exhaustive pattern match in case");}();
      var m1__35=function(){
      switch(Tuple2$bm1__33[0]){
       case "Tuple2":
       return Tuple2$bm1__33[2];
      }
      throw new Error("Non-exhaustive pattern match in case");}();
      return function(){
       var Tuple2$cm2__40=m2_29(b_34);
       var c_41=function(){
       switch(Tuple2$cm2__40[0]){
        case "Tuple2":
        return Tuple2$cm2__40[1];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       var m2__42=function(){
       switch(Tuple2$cm2__40[0]){
        case "Tuple2":
        return Tuple2$cm2__40[2];
       }
       throw new Error("Non-exhaustive pattern match in case");}();
       return ["Tuple2",c_41,$op['>>>'](m1__35)(m2__42)];}();}();});}();};};
 $op['<<<'] = function(a2_47){
  return function(a1_48){
   return $op['>>>'](a1_48)(a2_47);};};
 $op['^>>'] = function(f_49){
  return function(a_50){
   return $op['>>>'](pure_4(f_49))(a_50);};};
 $op['>>^'] = function(a_51){
  return function(f_52){
   return $op['>>>'](a_51)(pure_4(f_52));};};
 $op['^<<'] = function(f_53){
  return function(a_54){
   return $op['>>>'](a_54)(pure_4(f_53));};};
 $op['<<^'] = function(a_55){
  return function(f_56){
   return $op['>>>'](pure_4(f_56))(a_55);};};
 var count_7=init_5(0)(function(__84){
  return function(c_85){
   return (1+c_85);};});
 function run_1(Automaton$m0_14){
  return function(input_15){
   return function(){
   switch(Automaton$m0_14[0]){
    case "Automaton":
    return lift(fst)(foldp$(function(a_17){
     return function(Tuple2$bAutomaton$m_18){
      return function(){
      switch(Tuple2$bAutomaton$m_18[0]){
       case "Tuple2":
       switch(Tuple2$bAutomaton$m_18[2][0]){
        case "Automaton":
        return Tuple2$bAutomaton$m_18[2][1](a_17);
       }break;
      }
      throw new Error("Non-exhaustive pattern match in case");}();};})(Automaton$m0_14[1])(input_15));
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function step_2(Automaton$m_21){
  return function(a_22){
   return function(){
   switch(Automaton$m_21[0]){
    case "Automaton":
    return Automaton$m_21[1](a_22);
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function combine_3(autos_57){
  return Automaton_0(function(a_58){
   return function(){
    var Tuple2$bsautos__59=unzip(map(function(Automaton$m_62){
     return function(){
     switch(Automaton$m_62[0]){
      case "Automaton":
      return Automaton$m_62[1](a_58);
     }
     throw new Error("Non-exhaustive pattern match in case");}();})(autos_57));
    var bs_60=function(){
    switch(Tuple2$bsautos__59[0]){
     case "Tuple2":
     return Tuple2$bsautos__59[1];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    var autos__61=function(){
    switch(Tuple2$bsautos__59[0]){
     case "Tuple2":
     return Tuple2$bsautos__59[2];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
    return ["Tuple2",bs_60,combine_3(autos__61)];}();});}
 function pure_4(f_68){
  return Automaton_0(function(x_69){
   return ["Tuple2",f_68(x_69),pure_4(f_68)];});}
 function init_5(s_70){
  return function(step_71){
   return Automaton_0(function(a_72){
    return function(){
     var s__73=step_71(a_72)(s_70);
     return ["Tuple2",s__73,init_5(s__73)(step_71)];}();});};}
 function init__6(s_74){
  return function(step_75){
   return Automaton_0(function(a_76){
    return function(){
     var Tuple2$bs__77=step_75(a_76)(s_74);
     var b_78=function(){
     switch(Tuple2$bs__77[0]){
      case "Tuple2":
      return Tuple2$bs__77[1];
     }
     throw new Error("Non-exhaustive pattern match in case");}();
     var s__79=function(){
     switch(Tuple2$bs__77[0]){
      case "Tuple2":
      return Tuple2$bs__77[2];
     }
     throw new Error("Non-exhaustive pattern match in case");}();
     return ["Tuple2",b_78,init__6(s__79)(step_75)];}();});};}
 function vecSub_11(Tuple2$x1y1_86){
  return function(Tuple2$x2y2_87){
   return function(){
   switch(Tuple2$x1y1_86[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$x2y2_87[0]){
     case "Tuple2":
     return ["Tuple2",(Tuple2$x1y1_86[1]-Tuple2$x2y2_87[1]),(Tuple2$x1y1_86[2]-Tuple2$x2y2_87[2])];
    }
    throw new Error("Non-exhaustive pattern match in case");}();
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function stepDrag_12(Tuple2$presspos_92){
  return function(Tuple2$dsform_93){
   return function(){
   switch(Tuple2$presspos_92[0]){
    case "Tuple2":
    return function(){
    switch(Tuple2$dsform_93[0]){
     case "Tuple2":
     return function(){
      function wrap_98(ds__99){
       return ["Tuple2",Tuple2$dsform_93[2],["Tuple2",ds__99,Tuple2$dsform_93[2]]];}
      return function(){
      switch(Tuple2$dsform_93[1][0]){
       case "DragFrom":
       return (Tuple2$presspos_92[1]?["Tuple2",uncurry(move)(vecSub_11(Tuple2$presspos_92[2])(Tuple2$dsform_93[1][1]))(Tuple2$dsform_93[2]),["Tuple2",DragFrom_10(Tuple2$dsform_93[1][1]),Tuple2$dsform_93[2]]]:function(){
        var form__101=uncurry(move)(vecSub_11(Tuple2$presspos_92[2])(Tuple2$dsform_93[1][1]))(Tuple2$dsform_93[2]);
        return ["Tuple2",form__101,["Tuple2",Listen_8,form__101]];}());
       case "Ignore":
       return wrap_98((Tuple2$presspos_92[1]?Ignore_9:Listen_8));
       case "Listen":
       return wrap_98((not(Tuple2$presspos_92[1])?Listen_8:(isWithin(Tuple2$presspos_92[2])(Tuple2$dsform_93[2])?DragFrom_10(Tuple2$presspos_92[2]):Ignore_9)));
      }
      throw new Error("Non-exhaustive pattern match in case");}();}();
    }
    throw new Error("Non-exhaustive pattern match in case");}();
   }
   throw new Error("Non-exhaustive pattern match in case");}();};}
 function draggable_13(form_102){
  return init__6(["Tuple2",Listen_8,form_102])(stepDrag_12);}
 return {$op : {'>>>' : $op['>>>'], '<<<' : $op['<<<'], '^>>' : $op['^>>'], '>>^' : $op['>>^'], '^<<' : $op['^<<'], '<<^' : $op['<<^']},
 run:run_1,
 step:step_2,
 combine:combine_3,
 pure:pure_4,
 init:init_5,
 init$:init__6,
 count:count_7,
 draggable:draggable_13};}();
Elm.main=function(){
 return Elm.Automaton.main;};
} catch (e) {
Elm.main=function() {
var msg = ('<br/><h2>Your browser may not be supported. Are you using a modern browser?</h2>' + '<br/><span style="color:grey">Runtime Error in Automaton module:<br/>' + e + '</span>');
document.body.innerHTML = Elm.Text.monospace(msg);throw e;};}}());