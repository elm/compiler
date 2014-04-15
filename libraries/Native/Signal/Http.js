Elm.Native.Http = {};
Elm.Native.Http.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Http = elm.Native.Http || {};
    if (elm.Native.Http.values) return elm.Native.Http.values;

    var List = Elm.List.make(elm);
    var Signal = Elm.Signal.make(elm);

    function registerReq(queue,responses) {
        return function(req) {
            if (req.url.length > 0) {
                sendReq(queue,responses,req);
            }
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

        var request = (window.ActiveXObject
                       ? new ActiveXObject("Microsoft.XMLHTTP")
                       : new XMLHttpRequest());

        request.onreadystatechange = function(e) {
            if (request.readyState === 4) {
                response.value = (request.status >= 200 && request.status < 300 ?
                                  { ctor:'Success', _0:request.responseText } :
                                  { ctor:'Failure', _0:request.status, _1:request.statusText });
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        };
        request.open(req.verb, req.url, true);
        function setHeader(pair) {
            request.setRequestHeader( pair._0, pair._1 );
        }
        A2( List.map, setHeader, req.headers );
        request.send(req.body);
    }

    function send(requests) {
        var responses = Signal.constant(elm.Http.values.Waiting);
        var sender = A2( Signal.lift, registerReq([],responses), requests );
        function f(x) { return function(y) { return x; } }
        return A3( Signal.lift2, f, responses, sender );
    }

    function registerJsonp(queue,responses) {
        return function(url) {
            if (url.length > 0) {
                jsonp(queue,responses,url);
            }
        };
    }

    var rjsonp = /(=)\?(?=&|$)|\?\?/;
    function jsonp(queue,responses,url) {
        var response = { value: { ctor:'Waiting' } };
        queue.push(response);

        var callbackName = 'jsonpCallback_' + new String(Math.random()).replace(/\D/g, '');
        window[callbackName] = function(data) {
            response.value = { ctor:'Success', _0:data };
            setTimeout(function() { updateQueue(queue,responses); }, 0);

            try {
                delete window[callbackName];
                script.parentNode.removeChild(script);
            } catch (e) {}
        };

        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.async = true;
        script.src = url.replace(rjsonp, '$1' + callbackName);

        document.getElementsByTagName('head')[0].appendChild(script);
    }

    function sendJsonp(urls) {
        var responses = Signal.constant(elm.Http.values.Waiting);
        var sender = A2( Signal.lift, registerJsonp([],responses), urls );
        function f(x) { return function(y) { return x; } }
        return A3( Signal.lift2, f, responses, sender );
    }

    return elm.Native.Http.values = {
        send: send,
        sendJsonp: sendJsonp
    };
};
