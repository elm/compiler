Elm.Native.Lazy = {};
Elm.Native.Lazy.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Lazy = elm.Native.Lazy || {};

    if (elm.Native.Lazy.values) return elm.Native.Lazy.values;

    var Utils = Elm.Native.Utils.make(elm);
    
    function thunk(f) {
	var value = {};
	var isEvaled = false;
	var memoed = function(x) {
	    if(!isEvaled) {
		value = f(Utils.Tuple0);
		isEvaled = true;
	    }
	    return value;
	}
	return memoed;
    }

    return elm.Native.Lazy.values = {
	thunk : thunk
    };
};
