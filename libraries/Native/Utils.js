
Elm.Native.Utils = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Utils) return elm.Native.Utils;

  function eq(x,y) {
    if (x === y) return true;
    if (typeof x === "object") {
      var c = 0;
      for (var i in x) { ++c; if (!eq(x[i],y[i])) return false; }
      return c === Object.keys(y).length;
    }
    if (typeof x === 'function') {
      throw new Error('Equality error: general function equality is ' +
      'undecidable, and therefore, unsupported');
    }
    return x === y;
  }

  var EQ = 0, LT = 1, GT = 2, ord = ['EQ','LT','GT'];
  function compare(x,y) { return { ctor: ord[cmp(x,y)] } }
  function cmp(x,y) {
    var ord;
    if (typeof x !== 'object') return x === y ? EQ : x < y ? LT : GT;

    if (x.ctor === "Cons" || x.ctor === "Nil") {
      while (true) {
          if (x.ctor === "Nil" && y.ctor === "Nil") return EQ;
          if (x.ctor !== y.ctor) return x.ctor === 'Nil' ? LT : GT;
          ord = cmp(x._0, y._0);
          if (ord !== EQ) return ord;
          x = x._1;
          y = y._1;
      }
    }

    if (x.ctor.slice(0,5) === 'Tuple') {
      var n = x.ctor.slice(5) - 0;
      var err = 'cannot compare tuples with more than 6 elements.';
      if (n === 0) return EQ;
      if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
      if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
      if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
      if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
      if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
      if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
      if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
      return EQ;
    }
    throw new Error('Comparison error: comparison is only defined on ints, ' +
        'floats, times, chars, strings, lists of comparable values, ' +
        'and tuples of comparable values.')
  }


  var Tuple0 = { ctor: "Tuple0" };
  function Tuple2(x,y) { return { ctor:"Tuple2", _0:x, _1:y } }

  var count = 0;
  function guid(_) { return count++ }

  function copy(r) {
    var o = {};
    for (var i in r) { o[i] = r[i]; }
    return o;
  }

  function remove(x,r) {
    var o = copy(r);
    if (x in o._) {
      o[x] = o._[x][0];
      o._[x] = o._[x].slice(1);
      if (o._[x].length === 0) { delete o._[x]; }
    } else {
      delete o[x];
    }
    return o;
  }

  function replace(kvs,r) {
    var o = copy(r);
    for (var i = kvs.length; i--; ) {
      var kvsi = kvs[i];
      o[kvsi[0]] = kvsi[1];
    }
    return o;
  }

  function insert(x,v,r) {
    var o = copy(r);
    if (x in o) o._[x] = [o[x]].concat(x in o._ ? o._[x].slice(0) : []);
    o[x] = v;
    return o;
  }

  function max(a,b) { return a > b ? a : b }
  function min(a,b) { return a < b ? a : b }

  function htmlHeight(width, html) {
    var t = document.createElement('div');
    t.innerHTML = html;
    if (width > 0) { t.style.width = width + "px"; }
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";

    elm.node.appendChild(t);
    var w = t.clientWidth;
    var h = t.clientHeight;
    elm.node.removeChild(t);
    return Tuple2(w,h);
  }

  return elm.Native.Utils = {
      eq:eq,
      cmp:compare,
      compare:F2(compare),
      Tuple0:Tuple0,
      Tuple2:Tuple2,
      copy: copy,
      remove: remove,
      replace: replace,
      insert: insert,
      guid: guid,
      max : F2(max),
      min : F2(min),
      htmlHeight: F2(htmlHeight),
      toFloat: function(x){return x}
  };
};
