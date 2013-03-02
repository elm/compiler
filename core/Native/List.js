/**
module Native.List where

import Native.Function
**/

(function() {
  "use strict";

  var Nil = { ctor:'Nil' };
  function Cons(hd,tl) { return { ctor:"Cons", _0:hd, _1:tl }; }

  function throwError(f) {
    throw new Error("Function '" + f + "' expects a non-empty list!");
  }

  function toArray(xs) {
    var out = [];
    while (xs.ctor === "Cons") {
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
    if (xs.ctor === "Nil") { return ys; }
    var root = Elm.Native.List.Cons(xs._0. Elm.Native.List.Nil);
    var curr = root;
    xs = xs._1;
    while (xs.ctor==="Cons") {
	curr._1 = Elm.Native.List.Cons(xs._0. Elm.Native.List.Nil);
	xs = xs._1;
	curr = curr._1;
    }
    curr._1 = ys;
    return root;
  }

  function head(v) { v.ctor === "Nil" ? throwError('head') : return v._0; }
  function tail(v) { v.ctor === "Nil" ? throwError('tail') : return v._1; }

  function last(xs) {
    if (xs.ctor === "Nil") { throwError('last'); }
    var out = xs._0;
    while (xs.ctor === "Cons") {
      out = xs._0;
      xs = xs._1;
    }
    return out;
  }

  function map(f, xs) {
    var arr = [];
    while (xs.ctor === "Cons") {
      arr.push(f(xs._0));
      xs = xs._1;
    }
    return fromArray(arr);
  }

  function foldl(f, b, xs) {
    var acc = b;
    while (xs.ctor === "Cons") {
      acc = f(xs._0)(acc);
      xs = xs._1;
    }
    return acc;
  }

  function foldr(f, b, xs) {
    var arr = toArray(xs);
    var acc = b;
    for (var i = arr.length; i--; ) {
      acc = f(arr[i])(acc);
    }
    return acc;
  }

  function foldl1(f, xs) {
    xs.ctor === "Nil" ? throwError('foldl1') : return foldl(f, xs._0, xs._1);
  }

  function foldr1(f, xs) {
    if (xs.ctor === "Nil") { throwError('foldr1'); }
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
    return xs.ctor === "Nil" ? throwError('scanl1') : scanl(f, xs._0, xs._1);
  }

  function filter(pred, xs) {
    var arr = [];
    while (xs.ctor === "Cons") {
      if (pred(xs._0)) { arr.push(xs._0); }
      xs = xs._1;
    }
    return fromArray(arr);
  }

  function length(xs) {
    var out = 0;
    while (xs.ctor === "Cons") {
      out += 1;
      xs = xs._1;
    }
    return out;
  }

  function reverse(xs) { return fromArray(toArray(xs).reverse()); }
   
  function all(pred, xs) {
    while (xs.ctor === "Cons") {
      if (!pred(xs._0)) return false;
      xs = xs._1;
    }
    return true;
  }

  function any(pred, xs) {
    while (xs.ctor === "Cons") {
      if (pred(xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function zipWith(f, xs, ys) {
    var arr = [];
    while (xs.ctor === "Cons" && ys.ctor === "Cons") {
      arr.push(A2(f, xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function zip(xs, ys) {
    var arr = [];
    while (xs.ctor === "Cons" && ys.ctor === "Cons") {
      arr.push({ ctor:'Tuple2', _0:xs._0, _1:ys._0 });
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function sort(xs) {
    return fromArray(toArray(xs).sort(function(a,b) { return a - b}));
  }
  
  function take(n, xs) {
    var arr = [];
    while (xs.ctor === "Cons" && n > 0) {
      arr.push(xs._0);
      xs = xs._1;
      --n;
    }
    return fromArray(arr);
  }

  function drop(n, xs) {
    while (xs.ctor === "Cons" && n > 0) {
      xs = xs._1;
      --n;
    }
    return xs;
  }

  Elm.Native.List = {
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
      reverse:reverse,

      all:F2(all),
      any:F2(any),
      zipWith:F3(zipWith),
      zip:F2(zip),
      sort:sort,
      take:F2(take),
      drop:F2(drop)
  };

}());