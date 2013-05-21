
Elm.Native.Mouse = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Mouse) return elm.Native.Mouse;

  var Signal = Elm.Signal(elm);
  var Utils = Elm.Native.Utils(elm);

  var position  = Signal.constant(Utils.Tuple2(0,0));
  position.defaultNumberOfKids = 2;

  // do not move x and y into Elm. By setting their default number
  // of kids, it is possible to detatch the mouse listeners if
  // they are not needed.
  var x = A2( Signal.lift, function(p){return p._0}, position);
  x.defaultNumberOfKids = 0;
  var y = A2( Signal.lift, function(p){return p._1}, position);
  y.defaultNumberOfKids = 0;

  var isDown    = Signal.constant(false);
  var isClicked = Signal.constant(false);
  var clicks = Signal.constant(Utils.Tuple0);

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
    return Utils.Tuple2(posx-elm.node.offsetX, posy-elm.node.offsetY);
  }

  var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

  function click(e) {
    var hasListener1 = elm.notify(isClicked.id, true);
    var hasListener2 = elm.notify(clicks.id, Utils.Tuple0);
    elm.notify(isClicked.id, false);
    if (!hasListener1 && !hasListener2)
	node.removeEventListener('click', click);
  }

  function down(e) {
    var hasListener = elm.notify(isDown.id, true);
    if (!hasListener) node.removeEventListener('mousedown', down);
  }

  function up(e) {
    var hasListener = elm.notify(isDown.id, false);
    if (!hasListener) node.removeEventListener('mouseup', up);
  }

  function move(e) {
    var hasListener = elm.notify(position.id, getXY(e));
    if (!hasListener) node.removeEventListener('mousemove', move);
  }

  node.addEventListener('click'    , click);
  node.addEventListener('mousedown', down);
  node.addEventListener('mouseup'  , up);
  node.addEventListener('mousemove', move);

  return elm.Native.Mouse = {
      position: position,
      x:x,
      y:y,
      isClicked: isClicked,
      isDown: isDown,
      clicks: clicks
  };
};