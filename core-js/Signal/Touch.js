
Elm.Touch = function() {
  var touches = Elm.Signal.constant(["Nil"]);
  
  Value.addListener(document, "touchstart",  function(e) {
	  var ts = e.changedTouches;
	  var vs = touches.value;
	  for (var i = ts.length; i--; ) {
	      var t = ts[i];
	      vs = ["Cons", Value.tuple(t.pageX, t.pageY), vs];
	  }
	  Dispatcher.notify(touches.id, vs);
      });
  Value.addListener(document, "touchend",    function(e) {
      });
  Value.addListener(document, "touchcancel", function(e) {
      });
  Value.addListener(document, "touchleave",  function(e) {
      });
  Value.addListener(document, "touchmove",   function(e) {
      });

  return {};
}();