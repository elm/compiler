
/*
import Signal
*/

(function() {
  'use strict';

  function inRange(min, max) {
    return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
  }

  function randomize(min, max, signal) {
    function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
    return Elm.Signal.lift(f)(signal);
  }

  Elm.Native.Random = { inRange:F2(inRange), randomize: F3(randomize) };

}());