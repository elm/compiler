
Elm.Native.Window = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Window) return elm.Native.Window;

  var Signal = Elm.Signal(elm);
  var Misc = Elm.Native.Misc(elm);

  var dimensions = Signal.constant(Misc.Tuple2(window.innerWidth,window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  var width  = A2(Signal.lift, function(p){return p._0}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1}, dimensions);
  height.defaultNumberOfKids = 0;

  function resize(e) {
      var w = elm.node.getElementById('widthChecker').offsetWidth;
      var hasListener = elm.notify(dimensions.id,
				   Misc.Tuple2(w, window.innerHeight));
      if (!hasListener)
	  this.removeEventListener('resize',arguments.callee,false);
  }
  Misc.addListener(window, 'resize', resize);

  return elm.Native.Window = {
      dimensions:dimensions,
      width:width,
      height:height
  };

};
