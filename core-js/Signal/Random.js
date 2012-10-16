
Elm.Random = function() {
  var inRange = function(min) { return function(max) {
      return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
    };
  };
  var randomize = function(min) { return function(max) { return function(signal) {
      function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
      return Elm.Signal.lift(f)(signal);
    };
   };
  };
  return { inRange:inRange, randomize:randomize };
}();