
Elm.Native.Keyboard = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Keyboard) return elm.Native.Keyboard;

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);

  var keysDown = Signal.constant(NList.Nil);
  var lastKey = Signal.constant('\0');

  elm.addListener([keysDown.id], document, 'keydown', function down(e) {
          if (NList.member(e.keyCode)(keysDown.value)) return;
          elm.notify(keysDown.id, NList.Cons(e.keyCode, keysDown.value));
      });
  elm.addListener([keysDown.id], document, 'keyup', function up(e) {
          function notEq(kc) { return kc !== e.keyCode; }
          elm.notify(keysDown.id, NList.filter(notEq)(keysDown.value));
      });
  elm.addListener([keysDown.id], document, 'blur', function blur(e) {
          elm.notify(keysDown.id, NList.Nil);
      });
  elm.addListener([lastKey.id], document, 'keypress', function press(e) {
          elm.notify(lastKey.id, e.charCode || e.keyCode);
      });

  function keySignal(f) {
    var signal = Signal.dropRepeats(A2(Signal.lift, f, keysDown));
    keysDown.defaultNumberOfKids += 1;
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

  function is(key) { return keySignal(NList.member(key)); }

  return elm.Native.Keyboard = {
      isDown:is,
      directions:F4(dir),
      keysDown:keysDown,
      lastPressed:lastKey
  };

};
