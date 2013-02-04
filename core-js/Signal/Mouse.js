/*! Mouse
  !*/

Elm.Mouse = function() {
  /*[Position]*/

  /** position : Signal (Int,Int)
      The current mouse position.
  **/
  var position  = Elm.Signal.constant(Value.Tuple(0,0));
  position.defaultNumberOfKids = 2;

  /** x : Signal Int
      The current x-coordinate of the mouse.
  **/
  var x = Elm.Signal.lift(function(p){return p[1];})(position);
  x.defaultNumberOfKids = 0;

  /** y : Signal Int
      The current y-coordinate of the mouse.
  **/
  var y = Elm.Signal.lift(function(p){return p[2];})(position);
  y.defaultNumberOfKids = 0;

  /*[Button Status]*/

  /** isDown : Signal Bool
      The current state of the left mouse-button.
      True when the button is down, and false otherwise.
   **/
  var isDown    = Elm.Signal.constant(false);

  /** isClicked : Signal Bool
      True immediately after the left mouse-button has been clicked,
      and false otherwise.
   **/
  var isClicked = Elm.Signal.constant(false);

  /** clicks : Signal ()
      Always equal to unit. Event triggers on every mouse click.
   **/
  var clicks = Elm.Signal.constant(Value.Tuple());
  
  function getXY(e) {
    var posx = 0;
    var posy = 0;
    if (!e) e = window.event;
    if (e.pageX || e.pageY) {
	posx = e.pageX;
	posy = e.pageY;
    } else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft +
	  document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop +
	  document.documentElement.scrollTop;
    }
    return Value.Tuple(posx, posy);
  }

  Value.addListener(document, 'click', function(e) {
	  var hasListener1 = Dispatcher.notify(isClicked.id, true);
	  var hasListener2 = Dispatcher.notify(clicks.id, Value.Tuple());
	  Dispatcher.notify(isClicked.id, false);
	  if (!hasListener1 && !hasListener2)
		this.removeEventListener('click',arguments.callee,false);
	});
  Value.addListener(document, 'mousedown', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, true);
	  if (!hasListener)
		this.removeEventListener('mousedown',arguments.callee,false);
	});
  Value.addListener(document, 'mouseup', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, false);
	  if (!hasListener)
		this.removeEventListener('mouseup',arguments.callee,false);
	});
  Value.addListener(document, 'mousemove', function(e) {
	  var hasListener = Dispatcher.notify(position.id, getXY(e));
	  if (!hasListener)
		this.removeEventListener('mousemove',arguments.callee,false);
	});

  /** isClickedOn : Element -> (Element, Signal Bool)
      Determine whether an element has been clicked. The resulting pair
      is a signal of booleans that is true when its paired element has
      been clicked. The signal is True immediately after the left
      mouse-button has been clicked, and false otherwise.
   **/
  var clickedOn = function(elem) {
	var node = Render.render(elem);
	var click = Elm.Signal.constant(false);
	Value.addListener(node, 'click', function(e) {
		Dispatcher.notify(click.id, true);
		Dispatcher.notify(click.id, false);
	  });
	return Value.Tuple(Value.wrap(node), click);
  };
  return {position: position,
	  x:x,
	  y:y,
	  isClicked: isClicked,
	  isDown: isDown,
	  clicks: clicks,
	  isClickedOn: clickedOn
	  };
  }();