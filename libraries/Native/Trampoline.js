Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Trampoline = elm.Native.Trampoline || {};
    if (elm.Native.Trampoline.values) return elm.Native.Trampoline.values;

    var _E = Elm.Native.Error.make(elm),

    // trampoline : Trampoline a -> a
    trampoline = function(t) {
        var tramp = t;
        while(true) {
            switch(tramp._0.ctor) {
            case "Left":
                return tramp._0._0;
            case "Right":
                tramp = tramp._0._0({ctor: "_Tuple0"});
                continue;
            }
            _E.Case("Trampoline", "in Native.Trampoline.trampoline");
        }
    }

    return elm.Native.Trampoline.values = { trampoline: trampoline };
};
