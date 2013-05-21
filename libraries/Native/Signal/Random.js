
Elm.Native.Random = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Random) return elm.Native.Random;

  var Signal = Elm.Signal(elm);

  function range(min, max, signal) {
    function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
    return A2( Signal.lift, f, signal );
  }

  function flt(signal) {
    function f(x) { return Math.random(); }
    return A2( Signal.lift, f, signal );
  }

  return elm.Native.Random = { range: F3(range), float: flt };

};
