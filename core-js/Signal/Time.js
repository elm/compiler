/*! Time
Library for working with time. Type `Time` represents some number of
milliseconds.
!*/

Elm.Time = function() {

  /*[Times]*/

  /** hour, minute, second, ms : Time
      Units of time, making it easier to specify things like a
      half-second `(second / 2)`.
  **/

  function timeNow() { return (new window.Date).getTime(); }

  /*[Tickers]*/

  /** fps : Number -> Signal Time
      Takes desired number of frames per second (fps). The resulting signal
      gives a sequence of time deltas as quickly as possible until it reaches
      the desired FPS. A time delta is the time between the last frame and the
      current frame.
  **/

  /** fpsWhen : Number -> Signal Bool -> Signal Time
      Same as the fps function, but you can turn it on and off. Allows you
      to do brief animations based on user input without major ineffeciencies.
      The first time delta after a pause is always zero, no matter how long
      the pause was. This way summing the deltas will actually give the amount
      of time that the output signal has been running.
  **/
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
 
  /** every : Time -> Signal Time
      Takes a time interval t. The resulting signal is the current time,
      updated every t.
   **/
  function everyWhen(isOn) { return function(t) {
      var clock = Elm.Signal.constant(timeNow());
      function tellTime() { Dispatcher.notify(clock.id, timeNow()); }
      setInterval(tellTime, t);
      return clock;
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