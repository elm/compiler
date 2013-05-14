
Elm.Native.Prelude = function(elm) {
  'use strict';
  if (elm.Native.Prelude) return elm.Native.Prelude;

  var JS = Elm.JavaScript(elm);
  var Maybe = Elm.Maybe(elm);
  var Utils = Elm.Native.Utils(elm);
  var Char = Elm.Char(elm);

  function div(a,b) { return (a/b)|0; }
  function rem(a,b) { return a % b; }
  function mod(a,b) {
    var r = a % b;
    var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

    return m === b ? 0 : m;
  }
  function abs(x) { return x < 0 ? -x : x; }
  function logBase(base,n) { return Math.log(n) / Math.log(base); }
  function min(a,b) { return a < b ? a : b; }
  function max(a,b) { return a > b ? a : b; }
  function clamp(lo,hi,n) { return n < lo ? lo : n > hi ? hi : n; }
  function xor(a,b) { return a !== b; }
  function not(b) { return !b; }

  function truncate(n) { return n|0; }

  function id(n) { return n; }
  function flip(f,a,b) { return A2(f,b,a); }
  function curry(f,a,b) { return f(Utils.Tuple2(a,b)); }
  function uncurry(f,v) { return A2(f,v._0,v._1); }
  function fst(t) { return t._0; }
  function snd(t) { return t._1; }

  function readInt(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    for (var i = start; i < len; ++i) {
      if (!Char.isDigit(s[i])) { return Maybe.Nothing; }
    }
    return Maybe.Just(parseInt(s, 10));
  }

  function readFloat(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    var dotCount = 0;
    for (var i = start; i < len; ++i) {
      if (Char.isDigit(s[i])) { continue; }
      if (s[i] === '.') {
        dotCount += 1;
        if (dotCount <= 1) { continue; }
      }
      return Maybe.Nothing;
    }
    return Maybe.Just(parseFloat(s));
  }

  var prelude = {
      div:F2(div),
      rem:F2(rem),
      mod:F2(mod),

      pi:Math.PI,
      e:Math.e,
      cos:Math.cos,
      sin:Math.sin,
      tan:Math.tan,
      acos:Math.acos,
      asin:Math.asin,
      atan:Math.atan,
      atan2:F2(Math.atan2),

      sqrt:Math.sqrt,
      abs:abs,
      logBase:F2(logBase),
      min:F2(min),
      max:F2(max),
      clamp:F3(clamp),
      compare:Utils.compare,

      xor:F2(xor),
      not:not,
      otherwise:true,

      truncate:truncate,
      ceiling:Math.ceil,
      floor:Math.floor,
      round:Math.round,
      toFloat:id,

      readInt:readInt,
      readFloat:readFloat,

      id:id,
      flip:F3(flip),
      curry:F3(curry),
      uncurry:F2(uncurry),
      fst:fst,
      snd:snd
  };

  function add(Module) {
    var M = Module(elm);
    for (var k in M) { prelude[k] = M[k]; }
  }
  add(Elm.Native.Show);
  add(Elm.Signal);
  add(Elm.List);
  add(Elm.Maybe);
  add(Elm.Time);
  add(Elm.Graphics.Element);

  return elm.Native.Prelude = prelude;
};
