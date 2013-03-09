
Elm.Native.Keyboard.Raw = function(elm) {
  'use strict';

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);
  var List = Elm.List(elm);
  var Maybe = Elm.Maybe(elm);
  var Misc = Elm.Native.Misc(elm);

  var keysDown = Signal.constant(NList.Nil);
  var charPressed = Signal.constant(Maybe.Nothing);

  function down(e) {
      if (List.member(e.keyCode)(keysDown.value)) return;
      var list = NList.Cons(e.keyCode, keysDown.value);
      var hasListener = elm.notify(keysDown.id, list);
      if (!hasListener)
	  this.removeEventListener('keydown',arguments.callee,false);
  }
  function up(e) {
      function notEq(kc) { return kc !== e.keyCode; }
      var codes = List.filter(notEq)(keysDown.value);
      var hasListener = elm.notify(keysDown.id, codes);
      if (!hasListener)
	  this.removeEventListener('keyup',arguments.callee,false);
  }
  function blur(e) {
      var hasListener = elm.notify(keysDown.id, NList.Nil);
      if (!hasListener)
	  this.removeEventListener('blur',arguments.callee,false);
  }
  function press(e) {
      var next = Maybe.Just(e.charCode || e.keyCode);
      var hasListener = elm.notify(charPressed.id, next);
      elm.notify(charPressed.id, Maybe.Nothing);
      if (!hasListener)
	  this.removeEventListener('keypress',arguments.callee,false);
  }

  Misc.addListener(document, 'keydown' , down );
  Misc.addListener(document, 'keyup'   , up   );
  Misc.addListener(window  , 'blur'    , blur );
  Misc.addListener(document, 'keypress', press);

  return elm.Native.Keyboard.Raw = {
      keysDown:keysDown,
      charPressed:charPressed
  };

};