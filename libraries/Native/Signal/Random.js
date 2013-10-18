Elm.Native.Random = {};
Elm.Native.Random.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Random = elm.Native.Random || {};
    if (elm.Native.Random.values) return elm.Native.Random.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);

    function range(min, max, signal) {
        function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
        return A2( Signal.lift, f, signal );
    }

    function float_(signal) {
        function f(x) { return Math.random(); }
        return A2( Signal.lift, f, signal );
    }

    function floatList(signal) {
        function f(n) {
            if (n < 0) return List.Nil;
            var arr = new Array(n);
            for (var i = n; i--; ) {
                arr[i] = Math.random();
            }
            return List.fromArray(arr);
        }
        return A2( Signal.lift, f, signal );
    }

    return elm.Native.Random.values = {
        range: F3(range),
        float_: float_,
        floatList: floatList
    };

};
