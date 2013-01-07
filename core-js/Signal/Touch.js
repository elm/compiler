
Elm.Touch = function() {
  var touches = Elm.Signal.constant(["Nil"]);
  
  function touch(t) {
    return {_ : [true], x: [t.pageX], y: [t.pageY], id: [t.identifier] };
  }

  function listen(name) {
    function update(e) {
      var ts = Elm.JavaScript.castJSArrayToList(e.touches);
      var hasListener = Dispatcher.notify(touches.id, Elm.List.map(touch)(ts));
      if (!hasListener)
        return this.removeEventListener(name,arguments.callee,false);
      e.preventDefault();
    }
    Value.addListener(document, name, update);
  }

  listen("touchstart");
  listen("touchmove");
  listen("touchend");
  listen("touchcancel");
  listen("touchleave");

  return { touches: touches };
}();