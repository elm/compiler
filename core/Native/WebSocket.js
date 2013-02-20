
function nativeWebSocket(elm) {
  "use strict";
  var JS = Elm.JavaScript;

  function open(url) { return function(outgoing) {
    var incoming = Elm.Signal.constant(Elm.Native.List.Nil);
    var ws = new window.WebSocket(JS.castStringToJSString(url));

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      Dispatcher.notify(incoming.id, JS.castJSStringToString(event.data));
    };
    
    function send(msg) {
      var s = JS.castStringToJSString(msg);
      ready ? ws.send(s) : pending.push(s);
    }
    
    function take1(x) { return function(y) { return x; } }
    return Elm.Signal.lift2(take1)(incoming)(Elm.Signal.lift(send)(outgoing));
   };
  }

  elm.Native.WebSocket = {open:open};
}
