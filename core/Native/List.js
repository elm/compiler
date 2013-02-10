function nativeList(elm) {

  function throwError(f) {
    throw new Error("Function '" + f + "' expects a non-empty list!");
  }

  function toArray(xs) {
    var out = [];
    while (xs[0] === "Cons") {
      out.push(xs[1]);
      xs = xs[2];
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

  function head(v) { v[0] === "Nil" ? throwError('head') : return v[1]; }
  function tail(v) { v[0] === "Nil" ? throwError('tail') : return v[2]; }

  function last(xs) {
    if (xs[0] === "Nil") { throwError('last'); }
    var out = xs[1];
    while (xs[0] === "Cons") {
      out = xs[1];
      xs = xs[2];
    }
    return out;
  }

  function map(f) { return function(xs) {
    var arr = [];
    while (xs[0] === "Cons") {
      arr.push(f(xs[1]));
      xs = xs[2];
    }
    return fromArray(arr);
   }
  }

  function foldl(f) { return function(b) { return function(xs) {
     var acc = b;
     while (xs[0] === "Cons") {
       acc = f(xs[1])(acc);
       xs = xs[2];
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
    xs[0] === "Nil" ? throwError('foldl1') : return foldl(f)(xs[1])(xs[2]);
   }
  }

  function foldr1(f) { return function(xs) {
    if (xs[0] === "Nil") { throwError('foldr1'); }
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
    xs[0] === "Nil" ? throwError('scanl1') : return scanl(f)(xs[1])(xs[2]);
   }
  }

  function filter(pred) { return function(xs) {
    var arr = [];
    while (xs[0] === "Cons") {
      if (pred(xs[1])) { arr.push(xs[1]); }
      xs = xs[2];
    }
    return fromArray(arr);
   }
  }

  function length(xs) {
    var out = 0;
    while (xs[0] === "Cons") {
      out += 1;
      xs = xs[2];
    }
    return out;
  }

  function reverse(xs) { return fromArray(toArray(xs).reverse()); }
   
  function all(pred) { return function(xs) {
    while (xs[0] === "Cons") {
      if (!pred(xs[1])) return false;
      xs = xs[2];
    }
    return true;
   }
  }

  function any(pred) { return function(xs) {
    while (xs[0] === "Cons") {
      if (pred(xs[1])) return true;
      xs = xs[2];
    }
    return false;
   }
  }

  function zipWith(f) { return function(xs) { return function(ys) {
     var arr = [];
     while (xs[0] === "Cons" && ys[0] === "Cons") {
       arr.push(f(xs[1])(ys[1]));
       xs = xs[2];
       ys = ys[2];
     }
     return fromArray(arr);
    }
   }
  }

  function zip(xs) { return function(ys) {
    var arr = [];
    while (xs[0] === "Cons" && ys[0] === "Cons") {
      arr.push(["Tuple2",xs[1],ys[1]]);
      xs = xs[2];
      ys = ys[2];
    }
    return fromArray(arr);
   }
  }

  function sort(xs) {
    return fromArray(toArray(xs).sort(function(a,b) { return a - b}));
  }
  
  function take(n) { return function(xs) {
    var arr = [];
    while (xs[0] === "Cons" && n > 0) {
      arr.push(xs[1]);
      xs = xs[2];
      --n;
    }
    return fromArray(arr);
   }
  }

  function drop(n) { return function(xs) {
    while (xs[0] === "Cons" && n > 0) {
      xs = xs[2];
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