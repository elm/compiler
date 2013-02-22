/*
import Signal
import Keyboard.Raw
import List
*/

(function() {
  'use strict';

  function keySignal(f) {
    var signal = Elm.Signal.lift(f)(Elm.Keyboard.Raw.keysDown);
    Elm.Keyboard.Raw.keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(up) { return function(down) {
   return function(left) { return function(right) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks.ctor == "Cons") {
	switch (ks._0) {
	case left : --x; break;
	case right: ++x; break;
	case up   : ++y; break;
	case down : --y; break;
	}
	ks = ks._1;
      }
      return { _:{}, x:x, y:y };
    }
    return keySignal(f);
   }}}
  }

  function is(key) { return keySignal(Elm.List.member(key)); }

  Elm.Native.Keyboard = { isDown:is, dir:dir };

}());
