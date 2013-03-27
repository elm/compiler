Elm.Native.Show = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Show) return elm.Native.Show;

  var NList = Elm.Native.List(elm);
  var List = Elm.List(elm);
  var Maybe = Elm.Maybe(elm);
  var JS = Elm.JavaScript(elm);
  var Dict = Elm.Dict(elm);
  var Json = Elm.Json(elm);

  elm.node.addEventListener('log', function(e) { console.log(e.value); });
  elm.node.addEventListener('title', function(e) {document.title = e.value;});
  elm.node.addEventListener('redirect', function(e) {
	  if (e.value.length > 0) { window.location = e.value; }
      });
  elm.node.addEventListener('viewport', function(e) {
	  var node = document.getElementById('elm_viewport');
	  if (!node) {
	      node = document.createElement('meta');
	      node.id = 'elm_viewport';
	      node.name = 'viewport';
	      document.head.appendChild(node);
	  }
	  node.content = e.value;
	  Dispatcher.notify(elm.Window.dimensions.id,
			    Tuple2(elm.node.clientWidth, elm.node.clientHeight));
      });

  var toString = function(v) {
    if (typeof v === "undefined") {
       return "<undefined>";
    } else if (typeof v === "function") {
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
	    var output = [];
	    for (var k in v) {
		if (k === 'ctor') continue;
		output.push(toString(v[k]));
	    }
	    return "(" + output.join(",") + ")";
	} else if (v.ctor === "Cons") {
	    var isStr = typeof v._0 === "string",
	        start = isStr ? '"' : "[",
	        end   = isStr ? '"' : "]",
	        sep   = isStr ?  "" : ",",
	        f     = !isStr ? toString : function(x){
		return x === '\n' ? '\\n' : x;
	    };
	    var output = start + f(v._0);
	    v = v._1;
	    while (v.ctor === "Cons") {
		output += sep + f(v._0);
		v = v._1;
	    }
	    return output + end;
	} else if (v.ctor === "Nil") {
	    return "[]";
	} else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
	    var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
	    var list = A3(Dict.foldr, cons, NList.Nil, v);
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
  function show(v) { return NList.fromArray(toString(v)); }

  return elm.Native.Show = { show:show };
};