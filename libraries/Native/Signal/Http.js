Elm.Native.Http = {};
Elm.Native.Http.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Http = elm.Native.Http || {};
  if (elm.Native.Http.values) return elm.Native.Http.values;


  var JS = Elm.JavaScript.make(elm);
  var List = Elm.List.make(elm);
  var Signal = Elm.Signal.make(elm);


  function registerReq(queue,responses) { return function(req) {
    if (req.url.ctor !== '[]') { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
    if (queue.length > 0) {
      elm.notify(responses.id, queue[0].value);
      if (queue[0].value.ctor !== 'Waiting') {
        queue.shift();
        setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    }
  }

  function sendReq(queue,responses,req) {
    var response = { value: { ctor:'Waiting' } };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
      if (request.readyState === 4) {
        response.value = (request.status >= 200 && request.status < 300 ?
        { ctor:'Success', _0:JS.toString(request.responseText) } :
        { ctor:'Failure', _0:request.status, _1:JS.toString(request.statusText) });
        setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    };
    request.open(JS.fromString(req.verb), JS.fromString(req.url), true);
    function setHeader(pair) {
      request.setRequestHeader( JS.fromString(pair._0), JS.fromString(pair._1) );
    }
    List.map(setHeader)(req.headers);
    request.send(JS.fromString(req.body));
  }

  function send(requests) {
    var responses = Signal.constant(elm.Http.values.Waiting);
    var sender = A2( Signal.lift, registerReq([],responses), requests );
    function f(x) { return function(y) { return x; } }
    return A3( Signal.lift2, f, responses, sender );
  }

  return elm.Native.Http.values = {send:send};

};
