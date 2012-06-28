var List = function() {

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
    function foldr1(f_53) {
      return function(_temp_54) {
        return function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var x_55 = v[1];
            var xs_56 = v[2];
            return foldr(f_53)(x_55)(xs_56)
          }
        }(_temp_54)
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
    function forall(pred) {
	return foldl(function(x) { return function(acc) {
		    return acc && pred(x);
		};})(true);
    }
    function exists(pred) {
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
	    forall:forall,
	    exists:exists,
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
}();