/*
import Signal
import Native.Misc
*/

(function() {
  'use strict';

  var Misc = Elm.Native.Misc;

  var position  = Elm.Signal.constant(Misc.Tuple(0,0));
  position.defaultNumberOfKids = 2;

  // do not get rid of x and y. By setting their default number
  // of kids, it is possible to detatch the mouse listeners if
  // they are not needed.
  var x = Elm.Signal.lift(function(p){return p._0})(position);
  x.defaultNumberOfKids = 0;
  var y = Elm.Signal.lift(function(p){return p._1})(position);
  y.defaultNumberOfKids = 0;

  var isDown    = Elm.Signal.constant(false);
  var isClicked = Elm.Signal.constant(false);
  var clicks = Elm.Signal.constant(Misc.Tuple());
  
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
    return Misc.Tuple(posx, posy);
  }

  Misc.addListener(document, 'click', function(e) {
	  var hasListener1 = Dispatcher.notify(isClicked.id, true);
	  var hasListener2 = Dispatcher.notify(clicks.id, Misc.Tuple());
	  Dispatcher.notify(isClicked.id, false);
	  if (!hasListener1 && !hasListener2)
		this.removeEventListener('click',arguments.callee,false);
	});
  Misc.addListener(document, 'mousedown', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, true);
	  if (!hasListener)
		this.removeEventListener('mousedown',arguments.callee,false);
	});
  Misc.addListener(document, 'mouseup', function(e) {
	  var hasListener = Dispatcher.notify(isDown.id, false);
	  if (!hasListener)
		this.removeEventListener('mouseup',arguments.callee,false);
	});
  Misc.addListener(document, 'mousemove', function(e) {
	  var hasListener = Dispatcher.notify(position.id, getXY(e));
	  if (!hasListener)
		this.removeEventListener('mousemove',arguments.callee,false);
	});

  var clickedOn = function(elem) {
	var node = Render.render(elem);
	var click = Elm.Signal.constant(false);
	Misc.addListener(node, 'click', function(e) {
		Dispatcher.notify(click.id, true);
		Dispatcher.notify(click.id, false);
	  });
	return Misc.Tuple(Misc.wrap(node), click);
  };
  Elm.Native.Mouse = {position: position,
		      x:x,
		      y:y,
		      isClicked: isClicked,
		      isDown: isDown,
		      clicks: clicks,
		      isClickedOn: clickedOn};
}());