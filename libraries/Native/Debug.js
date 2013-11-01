Elm.Native.Debug = {};
Elm.Native.Debug.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    if (elm.Native.Debug.values) return elm.Native.Debug.values;
    if ('values' in Elm.Native.Debug)
        return elm.Native.Debug.values = Elm.Native.Debug.values;

    function tracePath(debugId, elem) {
        elem.props.debugTracePathId = debugId;
        return elem;
    }

    Elm.Native.Debug.values = {
        tracePath: F2(tracePath)
    };
    return elm.Native.Debug.values = Elm.Native.Debug.values;

};
