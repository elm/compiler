
/*
import Signal
*/

(function() {

  function inRange(min) { return function(max) {
      return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
    };
  }

  function randomize(min) { return function(max) { return function(signal) {
      function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
      return Elm.Signal.lift(f)(signal);
    };
   };
  }

  Elm.Native.Random = { inRange:inRange, randomize:randomize };

}());