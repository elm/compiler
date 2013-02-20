
Elm.Keyboard = { Raw : function() {
  var keysDown = Elm.Signal.constant(["Nil"]);
  var charPressed = Elm.Signal.constant(["Nothing"]);

  function remove(x,xs) {
	if (xs[0] === "Nil") return xs;
	if (xs[1] === x) return xs[2];
	return ["Cons", xs[1], remove(x,xs[2])];
  }
  function has(x,xs) {
	while (xs[0] !== "Nil") {
	  if (xs[1] === x) return true;
	  xs = xs[2];
	}
	return false;
  }
  Value.addListener(document, 'keydown', function(e) {
	  if (has(e.keyCode, keysDown.value)) return;
	  var hasListener = Dispatcher.notify(keysDown.id, ["Cons", e.keyCode, keysDown.value]);
	  if (!hasListener)
		this.removeEventListener('keydown',arguments.callee,false);
	});
  Value.addListener(document, 'keyup', function(e) {
	  var codes = remove(e.keyCode, keysDown.value);
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
	  var hasListener = Dispatcher.notify(charPressed.id, ["Just",e.charCode || e.keyCode]);
	  Dispatcher.notify(charPressed.id, ["Nothing"]);
	  if (!hasListener)
		this.removeEventListener('keypress',arguments.callee,false);
	});
  return {keysDown:keysDown,
	  charPressed:charPressed};
    }()
};

/*! Keyboard
These are nicely curated inputs from the keyboard. See the
[Keyboard.Raw library](/docs/Signal/KeyboardRaw.elm) for a
lower-level interface that will let you define more complicated behavior.
!*/

(function() {
  function keySignal(f) {
    var signal = Elm.Signal.lift(f)(Elm.Keyboard.Raw.keysDown);
    Elm.Keyboard.Raw.keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(left,right,up,down) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks[0] == "Cons") {
	switch (ks[1]) {
	case left : --x; break;
	case right: ++x; break;
	case up   : ++y; break;
	case down : --y; break;
	}
	ks = ks[2];
      }
      return { _:[true], x:[x], y:[y] };
    }
    return keySignal(f);
  }

  function is(key) {
    function f(ks) {
      while (ks[0] == "Cons") {
	if (key == ks[1]) return true;
	ks = ks[2];
      }
      return false;
    }
    return keySignal(f);
  }

  /*[Directions]*/

  /** arrows : Signal { x:Int, y:Int }
      A signal of records indicating which arrow keys are pressed.

      `{ x = 0, y = 0 }` when pressing no arrows.
      `{ x =-1, y = 0 }` when pressing the left arrow.
      `{ x = 1, y = 1 }` when pressing the up and right arrows.
      `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
  **/
  Elm.Keyboard.arrows = dir(37,39,38,40);

  /** wasd : Signal { x:Int, y:Int }
      Just like the arrows signal, but this uses keys w, a, s, and d,
      which are common controls for many computer games.
  **/
  Elm.Keyboard.wasd   = dir(65,68,87,83);

  /*[Modifiers]*/

  /** shift : Signal Bool
      Whether the shift key is pressed.
  **/
  Elm.Keyboard.shift  = is(16);

  /** ctrl : Signal Bool
      Whether the control key is pressed.
  **/
  Elm.Keyboard.ctrl   = is(17);

  /** space : Signal Bool
      Whether the space key is pressed.
  **/
  Elm.Keyboard.space  = is(32);

}());
