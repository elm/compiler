function nativeKeyboardRaw(elm) {
  var keysDown = Elm.Signal.constant(Elm.Native.List.Nil);
  var charPressed = Elm.Signal.constant(Elm.Maybe.Nothing);

  Value.addListener(document, 'keydown', function(e) {
	  if (Elm.List.member(e.keyCode)(keysDown.value)) return;
	  var list = Elm.Native.List.Cons(e.keyCode, keysDown.value);
	  var hasListener = Dispatcher.notify(keysDown.id, list);
	  if (!hasListener)
		this.removeEventListener('keydown',arguments.callee,false);
	});
  Value.addListener(document, 'keyup', function(e) {
	  function notEq(kc) { return kc !== e.keyCode; }
	  var codes = Elm.List.filter(notEq)(keysDown.value);
	  var hasListener = Dispatcher.notify(keysDown.id, codes);
	  if (!hasListener)
		this.removeEventListener('keyup',arguments.callee,false);
	});
  Value.addListener(window, 'blur', function(e) {
	  var hasListener = Dispatcher.notify(keysDown.id, ["Nil"]);
	  if (!hasListener)
		this.removeEventListener('blur',arguments.callee,false);
	});
  Value.addListener(document, 'keypress', function(e) {
	  var next = Elm.Maybe.Just(e.charCode || e.keyCode);
	  var hasListener = Dispatcher.notify(charPressed.id, next);
	  Dispatcher.notify(charPressed.id, Elm.Maybe.Nothing);
	  if (!hasListener)
		this.removeEventListener('keypress',arguments.callee,false);
	});

  elm.Native.Keyboard.Raw = {keysDown:keysDown, charPressed:charPressed};
}
