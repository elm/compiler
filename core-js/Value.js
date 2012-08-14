
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

  var properEscape = function(str) {
    return str.replace(/&/g,  "&#38;")
              .replace(/"/g, /*"*/  "&#34;")
              .replace(/'/g, /*'*/  "&#39;")
              .replace(/</g,  "&#60;")
              .replace(/>/g,  "&#62;")
              .replace(/ /g,  "&nbsp;")
              .replace(/\n/g, "<br/>");
  };

  var toText = function(elmList) {
    if (typeof elmList === "string") return elmList;
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return properEscape(a.join(''));
  };

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
  var show = function(v) {
      return Text.monospace(properEscape(toString(v)));
  };
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
  
  return {eq:eq,
	  str:str,
	  show:show,
	  Tuple:Tuple,
	  append:append,
	  listToArray:listToArray,
	  toText : toText,
	  properEscape : properEscape};
}();