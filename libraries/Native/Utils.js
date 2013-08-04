
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

  // code in Generate/JavaScript.hs depends on the particular
  // integer values assigned to LT, EQ, and GT
  var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];
  function compare(x,y) { return { ctor: ord[cmp(x,y)+1] } }
  function cmp(x,y) {
    var ord;
    if (typeof x !== 'object') return x === y ? EQ : x < y ? LT : GT;

    if (x.ctor === "::" || x.ctor === "[]") {
      while (true) {
          if (x.ctor === "[]" && y.ctor === "[]") return EQ;
          if (x.ctor !== y.ctor) return x.ctor === '[]' ? LT : GT;
          ord = cmp(x._0, y._0);
          if (ord !== EQ) return ord;
          x = x._1;
          y = y._1;
      }
    }

    if (x.ctor.slice(0,6) === '_Tuple') {
      var n = x.ctor.slice(6) - 0;
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


  var Tuple0 = { ctor: "_Tuple0" };
  function Tuple2(x,y) { return { ctor:"_Tuple2", _0:x, _1:y } }

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

  function mod(a,b) {
    var r = a % b;
    var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

    return m === b ? 0 : m;
  }

  function htmlHeight(width, html) {
    var t = document.createElement('div');
    t.innerHTML = html;
    if (width > 0) { t.style.width = width + "px"; }
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";

    elm.node.appendChild(t);
    var style = window.getComputedStyle(t, null);
    var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
    var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
    elm.node.removeChild(t);
    return Tuple2(w,h);
  }

  function adjustOffset() {
      var node = elm.node;
      var offsetX = 0, offsetY = 0;
      if (node.offsetParent) {
          do {
              offsetX += node.offsetLeft;
              offsetY += node.offsetTop;
          } while (node = node.offsetParent);
      }
      elm.node.offsetX = offsetX;
      elm.node.offsetY = offsetY;
  }

  if (elm.display === ElmRuntime.Display.COMPONENT) {
      elm.addListener(elm.inputs, elm.node, 'mouseover', adjustOffset);
  }

  return elm.Native.Utils = {
      eq:eq,
      cmp:cmp,
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
      mod : F2(mod),
      htmlHeight: F2(htmlHeight),
      toFloat: function(x){return x}
  };
};
