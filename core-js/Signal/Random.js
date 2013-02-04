/*! Random
  !*/

Elm.Random = function() {
  /*[In a Range]*/

  /** inRange : Int -> Int -> Signal Int
      Given a range from low to high, this produces a random number
      between 'low' and 'high' inclusive. The value in the signal does
      not change after the page has loaded.
  **/
  var inRange = function(min) { return function(max) {
      return Elm.Signal.constant(Math.floor(Math.random() * (max-min+1)) + min);
    };
  };

  /** randomize : Int -> Int -> Signal a -> Signal Int
      Given a range from low to high and a signal of values, this produces
      a new signal that changes whenever the input signal changes. The new
      values are random number between 'low' and 'high' inclusive.
  **/
  var randomize = function(min) { return function(max) { return function(signal) {
      function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
      return Elm.Signal.lift(f)(signal);
    };
   };
  };
  return { inRange:inRange, randomize:randomize };
}();