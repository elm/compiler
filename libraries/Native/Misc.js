
Elm.Native.Misc = function(elm) {
  'use strict';

  var NativeList = Elm.Native.List(elm);
  var List = Elm.List(elm);
  var Maybe = Elm.Maybe(elm);
  var JS = Elm.JavaScript(elm);
  var Dict = Elm.Dict(elm);
  var Json = Elm.Json(elm);

  function eq(x,y) {
    if (x === y) return true;
    if (typeof x === "object") {
      if ('_' in x) {
        for (var i in x) { if (!eq(x[i],y[i])) return false; }
	for (var i in y) { if (!(i in x)) return false; }
	return true;
      }
      if (x.ctor !== y.ctor) return false;
      for (var i = x.length; i--; ) {
	if (!eq(x[i],y[i])) return false;
      }
      return true;
    }
    return x === y;
  }

  var Tuple0 = { ctor: "Tuple0" }
  function Tuple2(x,y) { return { ctor = "Tuple2", _0:x, _1:y } }

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
    forms = JS.fromList(forms);
    var groups = [];
    var arr = [];
    for (var i = forms.length; i--; ) {
	var f = forms[i];
	switch(f._3.ctor) {
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
    } else if ('ctor' in v) {
	if (v.ctor.substring(0,5) === "Tuple") {
	    var output = new Array(v.length-1);
	    for (var i = v.length; --i; ) { output[i-1] = toString(v[i]); }
	    return "(" + output.join(",") + ")";
	} else if (v.ctor === "Cons") {
	    var isStr = typeof v._0 === "string";
	    var start = isStr ? '"' : "[";
	    var  end  = isStr ? '"' : "]";
	    var  sep  = isStr ?  "" : ",";
	    var   f   = isStr ? function(x){return x} : toString;
	    var output = start + f(v._0);
	    v = v._1;
	    while (v.ctor === "Cons") {
		output += sep + f(v._0);
		v = v._1;
	    }
	    return output + end;
	} else if (v.ctor === "Nil") {
	    return "[]";
	} else if (v.ctor === "JSON") {
	    return "(JSON.fromList " + toString(Json.toList(v)) + ")";
	} else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
	    function cons(k){ return function(v) { return function(acc) { return NativeList.Cons(Tuple2(k,v),acc); }; }; }
	    var list = Dict.foldr(cons)(NativeList.Nil)(v);
	    var name = "Dict";
	    if (list.ctor === "Cons" && list._0._1.ctor === "Tuple0") {
		name = "Set";
		list = A2(List.map, function(x){return x._0}, list);
	    }
	    return "(" + name + ".fromList " + toString(list) + ")";
	} else {
	    var output = "";
	    for (var i = v.length; --i; ) { output = ' ' + toString(v[i]) + output; }
	    output = v.ctor + output;
	    return (v.length > 1) ? "(" + output + ")" : output;
	}
    }
    return v+"";
  };
  var show = function(v) { return NativeList.fromArray(toString(v)); };

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

  return elm.Native.Misc = {
      eq:eq,
      show:show,
      Tuple0:Tuple0,
      Tuple2:Tuple2,
      toText : toText,
      getTextSize : getTextSize,
      getSize : getSize,
      getExcess : getExcess,
      groupForms : groupForms,
      addListener : addListener
  };
};