Elm.WebSocket = function() {
  var JS = Elm.JavaScript;

  /** send :: String -> Signal String -> Signal String
      Create a web-socket. The first argument is the URL of the desired
      web-socket server. The input signal holds the outgoing messages,
      and the resulting signal contains the incoming ones.
   **/
  function send(url) { return function(outgoing) {
      var incoming = Elm.Signal.constant(["Nil"]);
      var ws = new window.WebSocket(JS.castStringToJSString(url));

      var pending = [];
      var open = false;

      ws.onopen = function(e) {
	  var len = pending.length;
	  for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
	  open = true;
      };
      ws.onmessage = function(event) {
        Dispatcher.notify(incoming.id, JS.castJSStringToString(event.data));
      };

      function sendMsg(msg) {
	  var s = JS.castStringToJSString(msg);
	  open ? ws.send(s) : pending.push(s);
      }

      function take1(x) { return function(y) { return x; } }
      return Elm.Signal.lift2(take1)(incoming)(Elm.Signal.lift(sendMsg)(outgoing));
    };
  }

  return {send:send};
}();
