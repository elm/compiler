
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
    var Tuple2 = Elm.Native.Utils(elm).Tuple2;

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

    var showToString = function(v) {
        var result = "";
        if (typeof v === "function") {
            result = "<function>";
        } else if (typeof v === "boolean") {
            result = v ? "True" : "False";
        } else if (typeof v === "number") {
            result = v+"";
        } else if (typeof v === "string" && v.length < 2) {
            result = "'"+v+"'";
        } else if (typeof v === "object" && '_' in v) {
            var output = [];
            for (var k in v._) {
                console.log(k,v._[k]);
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + showToString(v._[k][i]));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + showToString(v[k]));
            }
            if (output.length === 0) result = "{}";
            result = "{ " + output.join(", ") + " }";
        } else if (typeof v === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,5) === "Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(showToString(v[k]));
                }
                result = "(" + output.join(",") + ")";
            } else if (v.ctor === "Cons") {
                var isStr = typeof v._0 === "string",
                start = isStr ? '"' : "[",
                end   = isStr ? '"' : "]",
                sep   = isStr ?  "" : ",",
                f     = !isStr ? showToString : function(x){
                    return x === '\n' ? '\\n' : x;
                };
                var output = start + f(v._0);
                v = v._1;
                while (v.ctor === "Cons") {
                    output += sep + f(v._0);
                    v = v._1;
                }
                result = output + end;
            } else if (v.ctor === "Nil") {
                result = "[]";
            } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
                var list = A3(Dict.foldr, cons, NList.Nil, v);
                var name = "Dict";
                if (list.ctor === "Cons" && list._0._1.ctor === "Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                result = name + ".fromList " + showToString(list);
            } else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = showToString(v[i]);
                    var parenless = str[0] === '{' || str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : '(' + str + ')');
                }
                result = v.ctor + output;
            }
        }

        if(result === "") {
          return v+"";
        } else {
          return result;
        }
    };

    var show = function(v) {
      return NList.fromArray([showToString(v)]);
    }

    return elm.Native.Show = { show:show };
};
