
Elm.Native.Window = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Window) return elm.Native.Window;

  var Signal = Elm.Signal(elm);
  var Tuple2 = Elm.Native.Utils(elm).Tuple2;

  var dimensions = Signal.constant(Tuple2(elm.node.clientWidth,
					  elm.node.clientHeight));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1}, dimensions);
  height.defaultNumberOfKids = 0;

  function resize(e) {
    console.log('use the base node (should happen after resize)');
    var hasListener = elm.notify(dimensions.id, Tuple2(elm.node.clientWidth,
						       elm.node.clientHeight));
    if (!hasListener) window.removeEventListener('resize', resize);
  }
  window.addEventListener('resize', resize);

  return elm.Native.Window = {
      dimensions:dimensions,
      width:width,
      height:height
  };

};
