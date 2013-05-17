
Elm.Native.Http = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Http) return elm.Native.Http;


  var JS = Elm.JavaScript(elm);
  var List = Elm.List(elm);
  var Signal = Elm.Signal(elm);


  function registerReq(queue,responses) { return function(req) {
    if (req.url !== "") { sendReq(queue,responses,req); }
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

  function setHeader(pair) {
    request.setRequestHeader( JS.fomString(pair._0), JS.fromString(pair._1) );
  }

  function sendReq(queue,responses,req) {
    var response = { value: { ctor:'Waiting' } };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
      if (request.readyState === 4) {
        response.value = (request.status === 200 ?
        { ctor:'Success', _0:JS.toString(request.responseText) } :
        { ctor:'Failure', _0:request.status, _1:JS.toString(request.statusText) });
        setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    };
    request.open(JS.fromString(req.verb), JS.fromString(req.url), true);
    List.map(setHeader)(req.headers);
    request.send(JS.fromString(req.body));
  }

  function send(requests) {
    var responses = Signal.constant(elm.Http.Waiting);
    var sender = A2( Signal.lift, registerReq([],responses), requests );
    function f(x) { return function(y) { return x; } }
    return A3( Signal.lift2, f, responses, sender );
  }

  return elm.Native.Http = {send:send};

};
