// requestAnimationFrame shim
// http://paulirish.com/2011/requestanimationframe-for-smart-animating/
// http://my.opera.com/emoller/blog/2011/12/20/requestanimationframe-for-smart-er-animating
// by Erik Möller, Paul Irish and Tino Zijdel
(function() {
    var lastTime = 0;
    var vendors = ['ms', 'moz', 'webkit', 'o'];
    for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
        window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
        window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                   || window[vendors[x]+'CancelRequestAnimationFrame'];
    }

    if (!window.requestAnimationFrame)
    window.requestAnimationFrame = function(callback, element) {
        var currTime = new Date().getTime();
        var timeToCall = Math.max(0, 16 - (currTime - lastTime));
        var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                                   timeToCall);
        lastTime = currTime + timeToCall;
        return id;
    };

    if (!window.cancelAnimationFrame) {
        window.cancelAnimationFrame = function(id) {
            clearTimeout(id);
        };
    }
}());

Elm.Time = function() {
  function animationFrameFPSWhen(desiredFPS) { return function (isOn) {
      var msPerFrame = 1000 / desiredFPS;
      if(desiredFPS == 0) { msPerFrame = 0; }
      var prev = timeNow(), curr = prev, diff = 0, wasOn = true;
      var ticker = Elm.Signal.constant(diff);
      function tick(zero) { return function (stamp) {
	  curr = timeNow();
	  diff = zero ? 0 : curr - prev;
	  prev = curr;
          Dispatcher.notify(ticker.id, diff);
      };};

      var timeoutID = 0;
      function f(isOn) { return function(t) {
        if (isOn) {
          timeoutID = setTimeout(function() {
            requestAnimationFrame(tick(!wasOn && isOn));
          }, msPerFrame);
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
	  animationFrameWhen : animationFrameFPSWhen(0),
	  animationFrame : animationFrameFPSWhen(0)(Elm.Signal.constant(true)),
	  animationFrameFPS : function(t) { return animationFrameFPSWhen(t)(Elm.Signal.constant(true)); },
	  animationFrameFPSWhen : function(t) { return animationFrameFPSWhen(t); },
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
