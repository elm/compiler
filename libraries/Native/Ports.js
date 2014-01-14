Elm.Native.Ports = {};
Elm.Native.Ports.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Ports = elm.Native.Ports || {};
    if (elm.Native.Ports.values) return elm.Native.Ports.values;

    var Signal = Elm.Signal.make(elm);

    function incomingSignal(converter) {
        return function(port) {
            var base = converter(port.internal.defaultValue);
            var signal = Signal.constant(base);
            port.internal.subscribe(function(v) {
                try {
                    elm.notify(signal.id, converter(v));
                } catch(e) {
                    port.internal.errorHandler(v);
                }
            });
            return signal;
        }
    }

    function outgoingSignal(converter) {
        return function(signal) {
            var subscribers = []
            function subscribe(handler) {
                subscribers.push(handler);
            }
            function unsubscribe(handler) {
                subscribers.pop(subscribers.indexOf(handler));
            }
            A2( Signal.lift, function(value) {
                var val = converter(value);
                var len = subscribers.length;
                for (var i = 0; i < len; ++i) {
                    subscribers[i](val);
                }
            }, signal);
            return { subscribe:subscribe, unsubscribe:unsubscribe };
        }
    }

    function portIn(name, converter) {
        var value = elm.ports.incoming[name];
        if (!value) {
            throw new Error("Initialization Error: port '" + name +
                            "' was not given an input!");
        }
        elm.ports.uses[name] += 1;
        try {
            return converter(value);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }
    }
    function portOut(name, converter, value) {
        try {
            elm.ports.outgoing[name] = converter(value);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }
        return value;
    }

    return elm.Native.Ports.values = {
        incomingSignal: incomingSignal,
        outgoingSignal: outgoingSignal,
        portOut: portOut,
        portIn: portIn
    };
};
