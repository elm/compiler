Elm.Native.Ports = {};
Elm.Native.Ports.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Ports = elm.Native.Ports || {};
    if (elm.Native.Ports.values) return elm.Native.Ports.values;

    var Signal = Elm.Signal.make(elm);

    // On failure, return message. On success, return the value in an array.
    // Wrapping in an array allows the programmer to pass in a null value.
    function processInput(converter, v) {
        try { var elmValue = converter(v); }
        catch(e) { return "The given value caused a runtime error!\n" + e.toString(); }

        var ctor = elmValue.ctor;
        if (ctor === 'Nothing' || ctor === 'Left') {
            return "The port's conversion function failed.";
        } else if (ctor === 'Just' || ctor === 'Right') {
            return [elmValue._0];
        }
        return [elmValue];
    }
    function portIn(name, converter) {
        var port = elm.ports.incoming[name];
        if (!port) {
            throw new Error("Initialization Error: port '" + name +
                            "' was not given an input!");
        }
        elm.ports.uses[name] += 1;
        var result = processInput(converter, port.internal.defaultValue);
        if (typeof result === 'string') {
            throw new Error("Initialization Error: The default value for port '" +
                            name + "' is invalid.\n" + result);
        }
        var signal = Signal.constant(result[0]);
        port.internal.subscribe(function(v) {
            var result = processInput(converter, v);
            if (typeof result === 'string') {
                port.internal.errorHandler(v)
            } else {
                elm.notify(signal.id, result[0]);
            }
        });
        return signal;
    }
    function portOut(name, signal) {
        var subscribers = []
        function subscribe(handler) {
            subscribers.push(handler);
        }
        function unsubscribe(handler) {
            subscribers.pop(subscribers.indexOf(handler));
        }
        A2( Signal.lift, function(value) {
            var len = subscribers.length;
            for (var i = 0; i < len; ++i) {
                subscribers[i](value);
            }
        }, signal);
        elm.ports.outgoing[name] = { subscribe:subscribe, unsubscribe:unsubscribe };
        return signal;
    }

    return elm.Native.Ports.values = {
        portOut: portOut,
        portIn: portIn
    };
};
