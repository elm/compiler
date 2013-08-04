
Elm.Native.Time = function(elm) {
  'use strict';

  var Signal = Elm.Signal(elm);
  var NS = Elm.Native.Signal(elm);
  var Maybe = Elm.Maybe(elm);
  var Utils = Elm.Native.Utils(elm);

  function fpsWhen(desiredFPS, isOn) {
    var msPerFrame = 1000 / desiredFPS;
    var prev = Date.now(), curr = prev, diff = 0, wasOn = true;
    var ticker = Signal.constant(diff);
    function tick(zero) { return function() {
        curr = Date.now();
        diff = zero ? 0 : curr - prev;
        prev = curr;
        elm.notify(ticker.id, diff);
      };
    }
    var timeoutID = 0;
    function f(isOn, t) {
      if (isOn) {
        timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
        clearTimeout(timeoutID);
      }
      wasOn = isOn;
      return t;
    }
    return A3( Signal.lift2, F2(f), isOn, ticker );
  }

  function everyWhen(t, isOn) {
    var clock = Signal.constant(Date.now());
    var id = setInterval(function tellTime() {
            if (!elm.notify(clock.id, Date.now())) {
                clearInterval(id);
            }
        }, t);
    return clock;
  }

  function since(t, s) {
    function cmp(a,b) { return !Utils.eq(a,b); }
    var dcount = Signal.count(A2(NS.delay, t, s));
    return A3( Signal.lift2, F2(cmp), Signal.count(s), dcount );
  }
  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
  }
  return elm.Native.Time = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Signal.constant(true)); },
      every : function(t) { return everyWhen(t, Signal.constant(true)) },
      delay : NS.delay,
      timestamp : NS.timestamp,
      since : F2(since),
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

};
