
Elm.Native.Keyboard = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Keyboard) return elm.Native.Keyboard;

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);

  var keysDown = Signal.constant(NList.Nil);
  var lastKey = Signal.constant('\0');

  function down(e) {
      if (NList.member(e.keyCode)(keysDown.value)) return;
      var list = NList.Cons(e.keyCode, keysDown.value);
      var hasListener = elm.notify(keysDown.id, list);
      if (!hasListener) elm.node.removeEventListener('keydown', down);
  }
  function up(e) {
      function notEq(kc) { return kc !== e.keyCode; }
      var codes = NList.filter(notEq)(keysDown.value);
      var hasListener = elm.notify(keysDown.id, codes);
      if (!hasListener) elm.node.removeEventListener('keyup', up);
  }
  function blur(e) {
      var hasListener = elm.notify(keysDown.id, NList.Nil);
      if (!hasListener) elm.node.removeEventListener('blur', blur);
  }
  function press(e) {
      var hasListener = elm.notify(lastKey.id, e.charCode || e.keyCode);
      if (!hasListener) elm.node.removeEventListener('keypress', press);
  }

  elm.node.addEventListener('keydown' , down );
  elm.node.addEventListener('keyup'   , up   );
  elm.node.addEventListener('blur'    , blur );
  elm.node.addEventListener('keypress', press);

  function keySignal(f) {
    var signal = A2( Signal.lift, f, keysDown );
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
      lastKey:lastKey
  };

};
