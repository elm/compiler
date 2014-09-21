Elm.Native.Window = {};
Elm.Native.Window.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Window = elm.Native.Window || {};
  if (elm.Native.Window.values) return elm.Native.Window.values;

  var Signal = Elm.Signal.make(elm);
  var NS = Elm.Native.Signal.make(elm);
  var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

  function getWidth() { return elm.node.clientWidth; }
  function getHeight() {
      if (elm.display === ElmRuntime.Display.FULLSCREEN) {
          return window.innerHeight;
      }
      return elm.node.clientHeight;
  }

  var dimensions = NS.input(Tuple2(getWidth(), getHeight()));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0;}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1;}, dimensions);
  height.defaultNumberOfKids = 0;

  function resizeIfNeeded() {
      // Do not trigger event if the dimensions have not changed.
      // This should be most of the time.
      var w = getWidth();
      var h = getHeight();
      if (dimensions.value._0 === w && dimensions.value._1 === h) return;

      setTimeout(function () {
          // Check again to see if the dimensions have changed.
          // It is conceivable that the dimensions have changed
          // again while some other event was being processed.
          var w = getWidth();
          var h = getHeight();
          if (dimensions.value._0 === w && dimensions.value._1 === h) return;
          elm.notify(dimensions.id, Tuple2(w,h));
      }, 0);
  }
  elm.addListener([dimensions.id], window, 'resize', resizeIfNeeded);

  return elm.Native.Window.values = {
      dimensions:dimensions,
      width:width,
      height:height,
      resizeIfNeeded:resizeIfNeeded
  };

};
