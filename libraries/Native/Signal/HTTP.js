
/*
import JavaScript
import List
import Signal
*/

(function() {
  'use strict';

  function registerReq(queue,responses) { return function(req) {
    if (req.url !== "") { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
    if (queue.length > 0) {
      Dispatcher.notify(responses.id, queue[0].value);
      if (queue[0].value.ctor !== Waiting) {
	queue.shift();
	setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    }
  }

  function setHeader(pair) {
    request.setRequestHeader(Elm.JavaScript.castStringToJSString(pair._0),
			     Elm.JavaScript.castStringToJSString(pair._1));
  }

  function sendReq(queue,responses,req) {
    var JS = Elm.JavaScript;
    var response = { value: Elm.HTTP.Waiting };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
      if (request.readyState === 4) {
        response.value = (request.status === 200
			  ? Elm.HTTP.Success(JS.castJSStringToString(request.responseText))
			  : Elm.HTTP.Failure(request.status)(JS.castJSStringToString(request.statusText)));
	setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    };
    request.open(JS.castStringToJSString(req.verb),
		 JS.castStringToJSString(req.url),
		 true);
    Elm.List.map(setHeader)(req.headers);
    request.send(JS.castStringToJSString(req.body));
  }
 
  function send(requests) {
    var responses = Elm.Signal.constant(Elm.HTTP.Waiting);
    var sender = Elm.Signal.lift(registerReq([],responses))(requests);
    function f(x) { return function(y) { return x; } }
    return Elm.Signal.lift2(f)(responses)(sender);
  }

  Elm.Native.HTTP = {send:send};
}());
