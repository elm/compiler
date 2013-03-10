
Elm.Native.Keyboard.Raw = function(elm) {
  'use strict';

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);
  var List = Elm.List(elm);
  var Maybe = Elm.Maybe(elm);

  var keysDown = Signal.constant(NList.Nil);
  var charPressed = Signal.constant(Maybe.Nothing);

  function down(e) {
      if (List.member(e.keyCode)(keysDown.value)) return;
      var list = NList.Cons(e.keyCode, keysDown.value);
      var hasListener = elm.notify(keysDown.id, list);
      if (!hasListener) elm.node.removeEventListener('keydown', down);
  }
  function up(e) {
      function notEq(kc) { return kc !== e.keyCode; }
      var codes = List.filter(notEq)(keysDown.value);
      var hasListener = elm.notify(keysDown.id, codes);
      if (!hasListener) elm.node.removeEventListener('keyup', up);
  }
  function blur(e) {
      var hasListener = elm.notify(keysDown.id, NList.Nil);
      if (!hasListener) elm.node.removeEventListener('blur', blur);
  }
  function press(e) {
      var next = Maybe.Just(e.charCode || e.keyCode);
      var hasListener = elm.notify(charPressed.id, next);
      elm.notify(charPressed.id, Maybe.Nothing);
      if (!hasListener) elm.node.removeEventListener('keypress', press);
  }

  elm.node.addEventListener('keydown' , down );
  elm.node.addEventListener('keyup'   , up   );
  elm.node.addEventListener('blur'    , blur );
  elm.node.addEventListener('keypress', press);

  return elm.Native.Keyboard.Raw = {
      keysDown:keysDown,
      charPressed:charPressed
  };

};