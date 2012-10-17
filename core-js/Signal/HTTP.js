Elm.HTTP = function() {
  var JS = Elm.JavaScript;
  var toElmString = Elm.JavaScript.castJSStringToString;
  function request(verb) { return function(url) { return function(data) {
    return function(headers) {
	return {0 : "Request",
		length : 1,
		verb : JS.castStringToJSString(verb),
		url : JS.castStringToJSString(url),
		data : data === null ? null : JS.castStringToJSString(data),
		headers : headers }; }; }; };
  }
  function get(url) { return request("GET")(url)(null)(["Nil"]); }
  function post(url) { return function(data) {
	  return request("POST")(url)(data)(["Nil"]); }; }

  function sendReq(responses) { return function(req) {
    Dispatcher.notify(responses.id,["Waiting"]);
    var request = null;
    if (window.XMLHttpRequest) {
	  request = new XMLHttpRequest();
    } else if (window.ActiveXObject) {
	  request = new ActiveXObject("Microsoft.XMLHTTP");
    }
    request.onreadystatechange = function(e) {
	if (request.readyState === 4) {
	  Dispatcher.notify(
	  responses.id,
	  request.status === 200
	  ? ["Success", toElmString(request.responseText)]
	  : ["Failure", request.status, toElmString(request.statusText)]);
	}
    };
    request.open(req.verb, req.url, true);
    Elm.List.map(function(pair) {
	    request.setRequestHeader(
		JS.castStringToJSString(pair[1]),
		JS.castStringToJSString(pair[2]));
	  })(req.headers);
    request.send(req.data);
    return null;
   };
  }

  function send(requests) {
    var responses = Elm.Signal.constant(["Waiting"]);
    var sender = Elm.Signal.lift(sendReq(responses))(requests);
    function f(x) { return function(y) { return x; } }
    var combine = Elm.Signal.lift2(f)(responses)(sender);
    return combine;
  }

  return {get   : get,
	  post  : post,
	  request : request,
	  send  : send,
	  sendGet : function(urls){return send(Elm.Signal.lift(get)(urls));}
	  };
}();
