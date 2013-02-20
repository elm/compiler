function nativeList(elm) {
  "use strict";

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
    var out = ["Nil"];
    for (var i = arr.length; i--; ) {
      out = ["Cons", arr[i], out];
    }
    return out;
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

  function map(f) { return function(xs) {
    var arr = [];
    while (xs.ctor === "Cons") {
      arr.push(f(xs._0));
      xs = xs._1;
    }
    return fromArray(arr);
   }
  }

  function foldl(f) { return function(b) { return function(xs) {
     var acc = b;
     while (xs.ctor === "Cons") {
       acc = f(xs._0)(acc);
       xs = xs._1;
     }
     return acc;
    }
   }
  }

  function foldr(f) { return function(b) { return function(xs) {
     var arr = toArray(xs);
     var acc = b;
     for (var i = arr.length; i--; ) {
       acc = f(arr[i])(acc);
     }
     return acc;
    }
   }
  }

  function foldl1(f) { return function(xs) {
    xs.ctor === "Nil" ? throwError('foldl1') : return foldl(f)(xs._0)(xs._1);
   }
  }

  function foldr1(f) { return function(xs) {
    if (xs.ctor === "Nil") { throwError('foldr1'); }
    var arr = toArray(xs);
    var acc = arr.pop();
    for (var i = arr.length; i--; ) {
	acc = f(arr[i])(acc);
    }
    return acc;
   }
  }

  function scanl(f) { return function(b) { return function(xs) {
     var arr = toArray(xs);
     arr.unshift(b);
     var len = arr.length;
     for (var i = 1; i < len; ++i) {
       arr[i] = f(arr[i])(arr[i-1]);
     }
     return fromArray(arr);
    }
   }
  }
  
  function scanl1(f) { return function(xs) {
    xs.ctor === "Nil" ? throwError('scanl1') : return scanl(f)(xs._0)(xs._1);
   }
  }

  function filter(pred) { return function(xs) {
    var arr = [];
    while (xs.ctor === "Cons") {
      if (pred(xs._0)) { arr.push(xs._0); }
      xs = xs._1;
    }
    return fromArray(arr);
   }
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
   
  function all(pred) { return function(xs) {
    while (xs.ctor === "Cons") {
      if (!pred(xs._0)) return false;
      xs = xs._1;
    }
    return true;
   }
  }

  function any(pred) { return function(xs) {
    while (xs.ctor === "Cons") {
      if (pred(xs._0)) return true;
      xs = xs._1;
    }
    return false;
   }
  }

  function zipWith(f) { return function(xs) { return function(ys) {
     var arr = [];
     while (xs.ctor === "Cons" && ys.ctor === "Cons") {
       arr.push(f(xs._0)(ys._0));
       xs = xs._1;
       ys = ys._1;
     }
     return fromArray(arr);
    }
   }
  }

  function zip(xs) { return function(ys) {
    var arr = [];
    while (xs.ctor === "Cons" && ys.ctor === "Cons") {
      arr.push(["Tuple2",xs._0,ys._0]);
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
   }
  }

  function sort(xs) {
    return fromArray(toArray(xs).sort(function(a,b) { return a - b}));
  }
  
  function take(n) { return function(xs) {
    var arr = [];
    while (xs.ctor === "Cons" && n > 0) {
      arr.push(xs._0);
      xs = xs._1;
      --n;
    }
    return fromArray(arr);
   }
  }

  function drop(n) { return function(xs) {
    while (xs.ctor === "Cons" && n > 0) {
      xs = xs._1;
      --n;
    }
    return xs;
   }
  }

  function concatA(

  elm.Native.List = {
      head:head,
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

      all:all,
      any:any,
      zipWith:zipWith,
      zip:zip,
      sort:sort,
      take:take,
      drop:drop};
}