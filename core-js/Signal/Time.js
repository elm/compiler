Elm.Time = function() {
  function timeNow() { return (new window.Date).getTime(); }
  function everyWhen(isOn) { return function(t) {
      var clock = Elm.Signal.constant(timeNow());
      function tellTime() { Dispatcher.notify(clock.id, timeNow()); }
      setInterval(tellTime, t);
      return clock;
    };
  }
  function fpsWhen(desiredFPS) { return function (isOn) {
      var msPerFrame = 1000 / desiredFPS;
      var prev = timeNow(), curr = prev, diff = 0, wasOn = true;
      var ticker = Elm.Signal.constant(diff);
      function tick(zero) { return function() {
	  curr = timeNow();
	  diff = zero ? 0 : curr - prev;
	  prev = curr;
	  Dispatcher.notify(ticker.id, diff);
        };
      }
      var timeoutID = 0;
      function f(isOn) { return function(t) {
	if (isOn) {
	    timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
	} else if (wasOn) {
	    clearTimeout(timeoutID);	    
	}
	wasOn = isOn;
	return t;
       };
      }
      return Elm.Signal.lift2(f)(isOn)(ticker);
    };
  }
  function since(t) { return function(s) {
	  function cmp(a) { return function(b) { return !Value.eq(a,b); }; }
	  var dcount = Elm.Signal.count(Elm.Signal.delay(t)(s));
	  return Elm.Signal.lift2(cmp)(Elm.Signal.count(s))(dcount);
      };
  }
  function after(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(false);
      setTimeout(function() { Dispatcher.notify(thread.id, true); }, t);
      return thread;
  }
  function before(t) {
      t *= 1000;
      var thread = Elm.Signal.constant(true);
      setTimeout(function() { Dispatcher.notify(thread.id, false); }, t);
      return thread;
  }
  function read(s) {
      var t = window.Date.parse(s);
      return isNaN(t) ? ["Nothing"] : ["Just",t];
  }
  return {fpsWhen : fpsWhen,
	  fps : function(t) { return fpsWhen(t)(Elm.Signal.constant(true)); },
	  every : everyWhen(Elm.Signal.constant(true)),
	  delay : Elm.Signal.delay,
	  since : since,
	  after  : after,
	  before : before,
	  hour   : 3600000,
	  minute : 60000,
	  second : 1000,
	  ms     : 1,
	  inHours   : function(t) { return t / 3600000; },
	  inMinutes : function(t) { return t / 60000; },
	  inSeconds : function(t) { return t / 1000; },
	  inMss     : function(t) { return t; },
	  toDate : function(t) { return new window.Date(t); },
	  read   : read
  };

}();