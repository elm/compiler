Elm.Time = function() {
  var now = function(t) {
      var clock = Elm.Signal.constant((new Date()).getTime());
      function tellTime() { Dispatcher.notify(clock.id, (new Date()).getTime()); }
      setInterval(tellTime, t);
      return clock;
  };
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
  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Nothing : Just(s);
  }
  return {now:now,
	  every:every,
	  after:after,
	  before:before,
	  hours : function(n) { return n * 3600000; },
	  minutes : function(n) { return n * 60000; },
	  seconds : function(n) { return n * 1000; },
	  millis : function(n) { return n; },
	  inHours : function(t) { return ~~(t / 3600000); },
	  inMinutes : function(t) { return ~~(t / 60000); },
	  inSeconds : function(t) { return ~~(t / 1000); },
	  inMillis : function(t) { return t; },
	  toDate : function(t) { return new Date(t); },
	  read : read
  };

}();