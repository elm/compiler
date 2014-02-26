Elm.Native.Debug = {};
Elm.Native.Debug.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    if (elm.Native.Debug.values) return elm.Native.Debug.values;

    var show = Elm.Native.Show.make(elm).show;

    function log(tag,value) {
        if (process && process.stdout) {
            process.stdout.write(tag + ': ' + show(value));
        } else {
            console.log(tag + ': ' + show(value));
        }
        return value;
    }

    return elm.Native.Debug.values = {
        log: F2(log)
    };
    
};
