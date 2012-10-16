Elm.Time = function() {
  var every = function(t) {
      t *= 1000;
      var clock = Elm.Signal.constant(0);
      var time = 0;
      setInterval(function() {
	      time += t;
	      Dispatcher.notify(clock.id, time/1000);
	  }, t);
      return clock;
  };
  var after = function(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(false);
      setTimeout(function() { Dispatcher.notify(thread.id, true); }, t);
      return thread;
  };
  var before = function(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(true);
      setTimeout(function() { Dispatcher.notify(thread.id, false); }, t);
      return thread;
  };
  return {every:every,after:after,before:before};
}();