Elm.Native.List = function(elm) {
  "use strict";

  elm.Native = elm.Native || {};
  if (elm.Native.List) return elm.Native.List;
  if ('values' in Elm.Native.List)
      return elm.Native.List = Elm.Native.List.values;

  var Utils = Elm.Native.Utils(elm);

  // TODO: Improve Nil handling
  // We can change places like:  if (xs.ctor === '[]') ... to if (xs === Nil) ...
  // but only if we're confident Nil can only be defined once.
  // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
  // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
  // So, this can only be fixed when modules initialisation is also fixed.
  // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
  // (on firefox 19)

  var Nil = { ctor:'[]' };

  // using freeze for every cons would be nice but is a huge (9x on firefox 19)
  // performance penalty
  function Cons(hd,tl) { return { ctor:"::", _0:hd, _1:tl }; }

  function throwError(f) {
    throw new Error("Function '" + f + "' expects a non-empty list!");
  }

  function toArray(xs) {
    var out = [];
    while (xs.ctor !== '[]') {
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
    if (xs.ctor === '[]') { return ys; }
    var root = Cons(xs._0, Nil);
    var curr = root;
    xs = xs._1;
    while (xs.ctor !== '[]') {
	curr._1 = Cons(xs._0, Nil);
	xs = xs._1;
	curr = curr._1;
    }
    curr._1 = ys;
    return root;
  }

  function head(v) { return v.ctor === '[]' ? throwError('head') : v._0; }
  function tail(v) { return v.ctor === '[]' ? throwError('tail') : v._1; }

  function last(xs) {
    if (xs.ctor === '[]') { throwError('last'); }
    var out = xs._0;
    while (xs.ctor !== '[]') {
      out = xs._0;
      xs = xs._1;
    }
    return out;
  }

  function map(f, xs) {
    var arr = [];
    while (xs.ctor !== '[]') {
      arr.push(f(xs._0));
      xs = xs._1;
    }
    return fromArray(arr);
  }

   // f defined similarly for both foldl and foldr (NB: different from Haskell)
   // ie, foldl : (a -> b -> b) -> b -> [a] -> b
  function foldl(f, b, xs) {
    var acc = b;
    while (xs.ctor !== '[]') {
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
    return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
  }

  function foldr1(f, xs) {
    if (xs.ctor === '[]') { throwError('foldr1'); }
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
    return xs.ctor === '[]' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
  }

  function filter(pred, xs) {
    var arr = [];
    while (xs.ctor !== '[]') {
      if (pred(xs._0)) { arr.push(xs._0); }
      xs = xs._1;
    }
    return fromArray(arr);
  }

  function length(xs) {
    var out = 0;
    while (xs.ctor !== '[]') {
      out += 1;
      xs = xs._1;
    }
    return out;
  }

  function member(x, xs) {
    while (xs.ctor !== '[]') {
      if (Utils.eq(x,xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function reverse(xs) { return fromArray(toArray(xs).reverse()); }

  function concat(xss) {
      if (xss.ctor === '[]') return xss;
      var arr = toArray(xss);
      var xs = arr[arr.length-1];
      for (var i = arr.length-1; i--; ) {
	  xs = append(arr[i], xs);
      }
      return xs;
  }

  function all(pred, xs) {
    while (xs.ctor !== '[]') {
      if (!pred(xs._0)) return false;
      xs = xs._1;
    }
    return true;
  }

  function any(pred, xs) {
    while (xs.ctor !== '[]') {
      if (pred(xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function zipWith(f, xs, ys) {
    var arr = [];
    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
      arr.push(A2(f, xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function zip(xs, ys) {
    var arr = [];
    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
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

  function nth(xs, n) {
    return toArray(xs)[n];
  }

  function take(n, xs) {
    var arr = [];
    while (xs.ctor !== '[]' && n > 0) {
      arr.push(xs._0);
      xs = xs._1;
      --n;
    }
    return fromArray(arr);
  }

  function drop(n, xs) {
    while (xs.ctor !== '[]' && n > 0) {
      xs = xs._1;
      --n;
    }
    return xs;
  }

  function join(sep, xss) {
    if (typeof sep === 'string') return toArray(xss).join(sep);
    if (xss.ctor === '[]') return Nil;
    var s = toArray(sep);
    var out = toArray(xss._0);
    xss = xss._1;
    while (xss.ctor !== '[]') {
      out = out.concat(s, toArray(xss._0));
      xss = xss._1;
    }
    return fromArray(out);
  }

  function split(seperator, list) {
    var array = toArray(list);
    var alen = array.length;
    if (alen === 0) {
      // splitting an empty list is a list of lists: [[]]
      return Cons(Nil,Nil);
    }

    var sep = toArray(seperator);
    var seplen = sep.length;
    if (seplen === 0) {
      // splitting with an empty sep is a list of all elements
      // Same as (map (\x -> [x]) list)
      var out = Nil;
      for (var i = alen; i--; ) {
        out = Cons(Cons(array[i],Nil), out);
      }
      return out;
    }

    var matches = [-seplen];
    var sepStart = sep[0];
    var len = alen - seplen + 1;
    for (var i = 0; i < len; ++i) {
      if (Utils.eq(array[i], sepStart)) {
        var match = true;
        for (var j = seplen; --j; ) {
          if (!Utils.eq(array[i+j], sep[j])) { match = false;  break; }
        }
        if (match) {
          matches.push(i);
          i += seplen - 1;
        }
      }
    }

    // shortcut in case of no matches
    if (matches.length === 0) {
      return Cons(list,Nil);
    }

    var out = Nil;
    var index = alen - 1;
    for (var i = matches.length; i--; ) {
      var temp = Nil;
      var stop = matches[i] + seplen - 1;
      for ( ; index > stop; --index ) {
        temp = Cons(array[index], temp);
      }
      out = Cons(temp,out);
      index -= seplen;
    }
    return out;
  }

  Elm.Native.List.values = {
      Nil:Nil,
      Cons:Cons,
      cons:F2(Cons),
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

      all:F2(all),
      any:F2(any),
      zipWith:F3(zipWith),
      zip:F2(zip),
      sort:sort,
      nth:F2(nth),
      take:F2(take),
      drop:F2(drop),

      join:F2(join),
      split:F2(split)
  };
  return elm.Native.List = Elm.Native.List.values;

};