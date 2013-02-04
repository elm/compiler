/*! WebSocket
A library for low latency HTTP communication. See the HTTP library for standard
requests like GET, POST, etc.
!*/

Elm.WebSocket = function() {
  var JS = Elm.JavaScript;

  /** open : String -> Signal String -> Signal String
      Create a web-socket. The first argument is the URL of the desired
      web-socket server. The input signal holds the outgoing messages,
      and the resulting signal contains the incoming ones.
   **/
  function open(url) { return function(outgoing) {
      var incoming = Elm.Signal.constant(["Nil"]);
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

  return {open:open};
}();
