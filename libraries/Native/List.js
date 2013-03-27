Elm.Native.List = function(elm) {
  "use strict";

  elm.Native = elm.Native || {};
  if (elm.Native.List) return elm.Native.List;
  if ('values' in Elm.Native.List) return elm.Native.List = Elm.Native.List.values

  var Utils = Elm.Native.Utils(elm);
   
  // TODO: Improve Nil handling
  // We can change places like:  if (xs.ctor === 'Nil') ... to if (xs === Nil) ...
  // but only if we're confident Nil can only be defined once.
  // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
  // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
  // So, this can only be fixed when modules initialisation is also fixed.
  // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
  // (on firefox 19)

  // TODO: Improve Nil handling
  // We can change places like:  if (xs.ctor === 'Nil') ... to if (xs === Nil) ...
  // but only if we're confident Nil can only be defined once.
  // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
  // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
  // So, this can only be fixed when modules initialisation is also fixed.
  // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
  // (on firefox 19)

  // freeze is universally supported and as a singleton introduces little performance penalty
  // for a small amount of object safety
  var Nil = Object.freeze({ ctor:'Nil' });

  // using freeze for every cons would be nice but is a huge (9x on firefox 19) performance penalty
  // undefined checking would also be nice but adds a 20% penalty
  // (it's much easier to catch errors at construction rather than usage time)
  function Cons(hd,tl) {
     /*
     if (typeof hd == "undefined")
        throw new Error("Cons: no head");
     if (typeof tl == "undefined")
        throw new Error("Cons: no tail");
      */
     return { ctor:"Cons", _0:hd, _1:tl }; }

  function throwError(f) {
    throw new Error("Function '" + f + "' expects a non-empty list!");
  }

  function toArray(xs) {
    var out = [];
    while (xs.ctor !== 'Nil') {
      out.push(xs._0);
      xs = xs._1;
    }
    return out;
  }

  function fromArray(arr) {
    var out = Nil;
    for (var i = arr.length; i--; ) {
      out = Cons(arr[i], out);
    }
    return out;
  }

  function range(lo,hi) {
    var lst = Nil;
    if (lo <= hi) {
      do { lst = Cons(hi,lst) } while (hi-->lo);
    }
    return lst
  }

  function append(xs,ys) {
    if (typeof xs === "string") { return xs.concat(ys); }
    if (xs.ctor === 'Nil') { return ys; }
    var root = Cons(xs._0, Nil);
    var curr = root;
    xs = xs._1;
    while (xs.ctor !== 'Nil') {
	curr._1 = Cons(xs._0, Nil);
	xs = xs._1;
	curr = curr._1;
    }
    curr._1 = ys;
    return root;
  }

  function head(v) { return v.ctor === 'Nil' ? throwError('head') : v._0; }
  function tail(v) { return v.ctor === 'Nil' ? throwError('tail') : v._1; }

  function last(xs) {
    if (xs.ctor === 'Nil') { throwError('last'); }
    var out = xs._0;
    while (xs.ctor !== 'Nil') {
      out = xs._0;
      xs = xs._1;
    }
    return out;
  }

  function map(f, xs) {
    var arr = [];
    while (xs.ctor !== 'Nil') {
      arr.push(f(xs._0));
      xs = xs._1;
    }
    return fromArray(arr);
  }

   // f defined similarly for both foldl and foldr (NB: different from Haskell)
   // ie, foldl :: (a -> b -> b) -> b -> [a] -> b
  function foldl(f, b, xs) {
    var acc = b;
    while (xs.ctor !== 'Nil') {
      acc = A2(f, xs._0, acc);
      xs = xs._1;
    }
    return acc;
  }

  function foldr(f, b, xs) {
    var arr = toArray(xs);
    var acc = b;
    for (var i = arr.length; i--; ) {
      acc = A2(f, arr[i], acc);
    }
    return acc;
  }

  function foldl1(f, xs) {
    return xs.ctor === 'Nil' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
  }

  function foldr1(f, xs) {
    if (xs.ctor === 'Nil') { throwError('foldr1'); }
    var arr = toArray(xs);
    var acc = arr.pop();
    for (var i = arr.length; i--; ) {
      acc = A2(f, arr[i], acc);
    }
    return acc;
  }

  function scanl(f, b, xs) {
    var arr = toArray(xs);
    arr.unshift(b);
    var len = arr.length;
    for (var i = 1; i < len; ++i) {
      arr[i] = A2(f, arr[i], arr[i-1]);
    }
    return fromArray(arr);
  }

  function scanl1(f, xs) {
    return xs.ctor === 'Nil' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
  }

  function filter(pred, xs) {
    var arr = [];
    while (xs.ctor !== 'Nil') {
      if (pred(xs._0)) { arr.push(xs._0); }
      xs = xs._1;
    }
    return fromArray(arr);
  }

  function length(xs) {
    var out = 0;
    while (xs.ctor !== 'Nil') {
      out += 1;
      xs = xs._1;
    }
    return out;
  }

  function member(x, xs) {
    while (xs.ctor !== 'Nil') {
      if (Utils.eq(x,xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function reverse(xs) { return fromArray(toArray(xs).reverse()); }

  function concat(xss) {
      if (xss.ctor === 'Nil') return xss;
      var arr = toArray(xss);
      var xs = arr[arr.length-1];
      for (var i = arr.length-1; i--; ) {
	  xs = append(arr[i], xs);
      }
      return xs;
  }

   function and(xs) {
      while (xs.ctor !== 'Nil') {
         if (!xs._0) return false;     // -- short circuit
         xs = xs._1;
      }
      return true;      // -- empty list is true (like Haskell)
   }

  function all(pred, xs) {
    while (xs.ctor !== 'Nil') {
      if (!pred(xs._0)) return false;
      xs = xs._1;
    }
    return true;
  }

   function or(xs) {
      while (xs.ctor !== 'Nil') {
         if (xs._0) return true;    // -- short circuit
         xs = xs._1;
      }
      return false;      // -- empty list is false (like Haskell)
   }

  function any(pred, xs) {
    while (xs.ctor !== 'Nil') {
      if (pred(xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function zipWith(f, xs, ys) {
    var arr = [];
    while (xs.ctor !== 'Nil' && ys.ctor !== 'Nil') {
      arr.push(A2(f, xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function zip(xs, ys) {
    var arr = [];
    while (xs.ctor !== 'Nil' && ys.ctor !== 'Nil') {
      arr.push(Utils.Tuple2(xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function sort(xs) {
    function cmp(a,b) {
      var ord = Utils.compare(a,b).ctor;
      return ord=== 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
    }
    return fromArray(toArray(xs).sort(cmp));
  }

  function take(n, xs) {
    var arr = [];
    while (xs.ctor !== 'Nil' && n > 0) {
      arr.push(xs._0);
      xs = xs._1;
      --n;
    }
    return fromArray(arr);
  }

  function drop(n, xs) {
    while (xs.ctor !== 'Nil' && n > 0) {
      xs = xs._1;
      --n;
    }
    return xs;
  }

  function join(sep, xss) {
    if (xss.ctor === 'Nil') return Nil;
    var s = toArray(sep);
    var out = toArray(xss._0);
    xss = xss._1;
    while (xss.ctor !== 'Nil') {
      out = out.concat(s, toArray(xss._0));
      xss = xss._1;
    }
    return fromArray(out);
  }

   function split(sep, xs) {
      var array = toArray(xs);
      var alen = array.length;
      if (alen == 0) {
         return Cons(Nil,Nil);     // splitting an empty list is a list of lists: [[]]
      }
      
      var s = toArray(sep);
      var slen = s.length;
      if (slen === 0) {
         var out = Nil;
         for (var i = alen; i--; ) {
            out = Cons(Cons(array[i],Nil), out);
         }
         return out;    // splitting with an empty sep is a list of all elements: map (\x -> [x]) xs
      }
      
      var runs = [];    // indices of matches:  array[runs[0]..] matches sep
      var c = s[0];     // base on 1st chr$ searching
      // Need to iterate forwards, otherwise split("aa", "aaa") = ["a",""] instead of ["","a"]
      for (var i = 0; i <= alen - slen; i++) {
         if (Utils.eq(array[i], c)) {     // 1st chr$ match
            var match = true;
            for (var j = slen; --j >= 1; ) {
               if (!Utils.eq(array[i+j], s[j])) { match = false;  break; }
            }
            if (match) {
               runs.push(i);
               i += slen -1;
            }
         }
      }
      
      if (runs.length === 0) {      // no matches shortcut
         return Cons(xs,Nil);
      }
      
      // eg, for split("aa","aaabaa"), runs = [0,4],
      // we want: [ "" (pre 0), "ab" (runs[0]+sep..runs[1]-1, "" (post runs[1]) ]

      var out = Nil;
      var stop = alen;
      for (var i=runs.length; --i>=0; ) {    // need to build list in reverse order
         var temp = Nil;
         var start = runs[i];
         for (var j = start+slen; j < stop; j++)
            temp = Cons(array[j], temp);
         
         out = Cons(temp, out);
         stop = start;
      }
      
      var temp = Nil;
      for (var i=runs[0]; --i>=0; ) {
         temp = Cons(array[i], temp);
      }
      out = Cons(temp,out);
      return out;
   }

  Elm.Native.List.values = {
      Nil:Nil,
      Cons:Cons,
      toArray:toArray,
      fromArray:fromArray,
      range:range,
      append:append,

      head:head,
      tail:tail,
      last:last,

      map:F2(map),
      foldl:F3(foldl),
      foldr:F3(foldr),

      foldl1:F2(foldl1),
      foldr1:F2(foldr1),
      scanl:F3(scanl),
      scanl1:F2(scanl1),
      filter:F2(filter),
      length:length,
      member:F2(member),
      reverse:reverse,
      concat:concat,

      and:and,
      all:F2(all),
      or:or,
      any:F2(any),
      zipWith:F3(zipWith),
      zip:F2(zip),
      sort:sort,
      take:F2(take),
      drop:F2(drop),

      join:F2(join),
      split:F2(split)
  };
  return elm.Native.List = Elm.Native.List.values;

};