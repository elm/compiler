
/**
import JavaScript
import List
import Signal
**/

Elm.Native.Signal.Http = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  elm.Native.Signal = elm.Native.Signal || {};
  if (elm.Native.Signal.Http) return elm.Native.Signal.Http;


  function registerReq(queue,responses) { return function(req) {
    if (req.url !== "") { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
    if (queue.length > 0) {
      elm.notify(responses.id, queue[0].value);
      if (queue[0].value.ctor !== Waiting) {
	queue.shift();
	setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    }
  }

  function setHeader(pair) {
    request.setRequestHeader(elm.JavaScript.fomString(pair._0),
			     elm.JavaScript.fromString(pair._1));
  }

  function sendReq(queue,responses,req) {
    var JS = elm.JavaScript;
    var response = { value: elm.Http.Waiting };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
      if (request.readyState === 4) {
        response.value = (request.status === 200
			  ? elm.Http.Success(JS.castJSStringToString(request.responseText))
			  : elm.Http.Failure(request.status)(JS.castJSStringToString(request.statusText)));
	setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    };
    request.open(JS.castStringToJSString(req.verb),
		 JS.castStringToJSString(req.url),
		 true);
    elm.List.map(setHeader)(req.headers);
    request.send(JS.castStringToJSString(req.body));
  }
 
  function send(requests) {
    var responses = elm.Signal.constant(elm.Http.Waiting);
    var sender = elm.Signal.lift(registerReq([],responses))(requests);
    function f(x) { return function(y) { return x; } }
    return elm.Signal.lift2(f)(responses)(sender);
  }

  return elm.Native.Signal.Http = {send:send};

};
