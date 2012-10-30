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

  function registerReq(queue,responses) { return function(req) {
    if (req.url !== "") { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
      if (queue.length > 0) {
	Dispatcher.notify(responses.id, queue[0].value);
        if (queue[0].value[0] !== "Waiting") {
	  queue.shift();
	  setTimeout(function() { updateQueue(queue,responses); }, 0);
	}
      }
  }

  function sendReq(queue,responses,req) {
    var response = { value: ["Waiting"] };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
	if (request.readyState === 4) {
	    response.value = (request.status === 200
			      ? ["Success", toElmString(request.responseText)]
			      : ["Failure", request.status,
				 toElmString(request.statusText)]);
	    setTimeout(function() { updateQueue(queue,responses); }, 0);
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
  }

  function send(requests) {
    var responses = Elm.Signal.constant(["Waiting"]);
    var sender = Elm.Signal.lift(registerReq([],responses))(requests);
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
