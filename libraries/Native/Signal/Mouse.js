
Elm.Native.Mouse = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Mouse) return elm.Native.Mouse;

  var Signal = Elm.Signal(elm);
  var Misc   = Elm.Native.Misc(elm);

  var position  = Signal.constant(Misc.Tuple(0,0));
  position.defaultNumberOfKids = 2;

  // do not get rid of x and y. By setting their default number
  // of kids, it is possible to detatch the mouse listeners if
  // they are not needed.
  var x = A2( Signal.lift, function(p){return p._0}, position);
  x.defaultNumberOfKids = 0;
  var y = A2( Signal.lift, function(p){return p._1}, position);
  y.defaultNumberOfKids = 0;

  var isDown    = Signal.constant(false);
  var isClicked = Signal.constant(false);
  var clicks = Signal.constant(Misc.Tuple0);
  
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
    return Misc.Tuple2(posx, posy);
  }

  function click(e) {
    var hasListener1 = elm.notify(isClicked.id, true);
    var hasListener2 = elm.notify(clicks.id, Misc.Tuple0);
    elm.notify(isClicked.id, false);
    if (!hasListener1 && !hasListener2)
	this.removeEventListener('click',arguments.callee,false);
  }

  function down(e) {
    var hasListener = elm.notify(isDown.id, true);
    if (!hasListener) this.removeEventListener('mousedown',arguments.callee,false);
  }

  function up(e) {
    var hasListener = elm.notify(isDown.id, false);
    if (!hasListener) this.removeEventListener('mouseup',arguments.callee,false);
  }

  function move(e) {
    var hasListener = elm.notify(position.id, getXY(e));
    if (!hasListener) this.removeEventListener('mousemove',arguments.callee,false);
  }

  Misc.addListener(elm.node, 'click'    , click);
  Misc.addListener(elm.node, 'mousedown', down);
  Misc.addListener(elm.node, 'mouseup'  , up);
  Misc.addListener(elm.node, 'mousemove', move);

  return elm.Native.Mouse = {
      position: position,
      x:x,
      y:y,
      isClicked: isClicked,
      isDown: isDown,
      clicks: clicks
  };
};