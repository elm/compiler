
Elm.Native.Window = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Window) return elm.Native.Window;

  var Signal = Elm.Signal(elm);
  var Tuple2 = Elm.Native.Utils(elm).Tuple2;

  function getWidth() { return elm.node.clientWidth; }
  function getHeight() {
      return document.body === elm.node ? window.innerHeight : elm.node.clientHeight;
  }

  var dimensions = Signal.constant(Tuple2(getWidth(), getHeight()));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0;}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1;}, dimensions);
  height.defaultNumberOfKids = 0;

  function resize(e) {
      var w = getWidth();
      var h = getHeight();
      if (dimensions.value._0 === w && dimensions.value._1 === h) return;
      var hasListener = elm.notify(dimensions.id, Tuple2(w,h));
      if (!hasListener) window.removeEventListener('resize', resize);
  }
  window.addEventListener('resize', resize);

  return elm.Native.Window = {
      dimensions:dimensions,
      width:width,
      height:height
  };

};
