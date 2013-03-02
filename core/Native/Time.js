/*
import Signal
import Maybe
*/

(function() {
  'use strict';

  function timeNow() { return (new window.Date).getTime(); }

  function fpsWhen(desiredFPS, isOn) {
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
    function f(isOn, t) {
      if (isOn) {
        timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
	clearTimeout(timeoutID);	    
      }
      wasOn = isOn;
      return t;
    }
    return Elm.Signal.lift2(F2(f))(isOn)(ticker);
  }
 
  function everyWhen(t, isOn) {
    var clock = Elm.Signal.constant(timeNow());
    function tellTime() { Dispatcher.notify(clock.id, timeNow()); }
    setInterval(tellTime, t);
    return clock;
  }

  function since(t, s) {
    function cmp(a,b) { return !Value.eq(a,b) }
    var dcount = Elm.Signal.count(Elm.Signal.delay(t)(s));
    return Elm.Signal.lift2(F2(cmp))(Elm.Signal.count(s))(dcount);
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
      return isNaN(t) ? Elm.Maybe.Nothing : Elm.Maybe.Just(t);
  }
  Elm.Native.Time = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Elm.Signal.constant(true)); },
      every : function(t) { return everyWhen(t, Elm.Signal.constant(true)) },
      delay : Elm.Signal.delay,
      since : F2(since),
      after  : after,
      before : before,
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

}());