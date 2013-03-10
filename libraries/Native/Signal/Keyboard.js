
Elm.Native.Keyboard = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Keyboard) return elm.Native.Keyboard;

  var Signal = Elm.Signal(elm);
  var KR = Elm.Keyboard.Raw(elm);
  var List = Elm.List(elm);

  function keySignal(f) {
    var signal = A2( Signal.lift, f, elm.Keyboard.Raw.keysDown );
    KR.keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(up, down, left, right) {
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
  }

  function is(key) { return keySignal(List.member(key)); }

  return elm.Native.Keyboard = { isDown:is, directions:F4(dir) };

};
