
Elm.Native.WebSocket = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.WebSocket) return elm.Native.WebSocket;

  var Signal = Elm.Signal(elm);
  var JS = Elm.JavaScript(elm);
  var List = Elm.Native.List(elm);

  function open(url, outgoing) {
    var incoming = Signal.constant(List.Nil);
    var ws = new WebSocket(JS.fromString(url));

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      elm.notify(incoming.id, JS.toString(event.data));
    };
    
    function send(msg) {
      var s = JS.fromString(msg);
      ready ? ws.send(s) : pending.push(s);
    }
    
    function take1(x,y) { return x }
    return A3(Signal.lift2, F2(take1), incoming, A2(Signal.lift, send, outgoing));
  }

  return elm.Native.WebSocket = { connect: F2(open) };
};
