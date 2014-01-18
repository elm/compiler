Elm.Native.Debug.Logging = {};
Elm.Native.Debug.Logging.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    elm.Native.Debug.Logging = elm.Native.Debug.Logging || {};
    if (elm.Native.Debug.Logging.values) return elm.Native.Debug.Logging.values;

    function unsafePrintLogMessage(str) {
        logMess(str);      
        return str;
    }

    function logMess(msg) {
        setTimeout(function() {
            throw new Error(msg);
        }, 0);
    }

    return Elm.Native.Debug.Logging.values = {
        unsafePrintLogMessage: unsafePrintLogMessage,
    };
};