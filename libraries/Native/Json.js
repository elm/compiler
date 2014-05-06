Elm.Native.Json = {};
Elm.Native.Json.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Json = elm.Native.Json || {};
    if (elm.Native.Json.values) return elm.Native.Json.values;

    var Maybe = Elm.Maybe.make(elm);
    var Dict = Elm.Dict.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function toJS(v) {
        switch (v.ctor) {
        case 'Null'   : return null;
        case 'String' : return v._0;
        case 'Number' : return v._0;
        case 'Boolean': return v._0;
        case 'Object' :
            var obj = {};
            var array = List.toArray(Dict.toList(v._0));
            for (var i = array.length; i--; ) {
                var entry = array[i];
                obj[entry._0] = toJS(entry._1);
            }
            return obj;
        case 'Array'  :
            var array = List.toArray(v._0);
            for (var i = array.length; i--; ) {
	        array[i] = toJS(array[i]);
            }
            return array;
        }
    }

    function toString(sep, value) {
        return JSON.stringify(toJS(value), null, sep);
    }

    function fromJS(v) {
        switch (typeof v) {
        case 'string' : return { ctor:"String" , _0: v };
        case 'number' : return { ctor:"Number" , _0: v };
        case 'boolean': return { ctor:"Boolean", _0: v };
        case 'object' :
            if (v === null) return { ctor:"Null" };
            if (v instanceof Array) {
                var array = new Array(v.length);
                for (var i = v.length; i--; ) {
                    array[i] = fromJS(v[i]);
                }
	        return {
                    ctor:"Array",
                    _0: List.fromArray(array)
                };
            }
            var array = [];
            for (var key in v) {
                var value = fromJS(v[key]);
                array.push(Utils.Tuple2(key, value));
            }
            var list = List.fromArray(array);
            return {
                ctor:"Object",
                _0: Dict.fromList(list)
            };
        }
    }

    function fromString(str) {
        try {
	    return Maybe.Just(fromJS(JSON.parse(str)));
        } catch (e) {
	    return Maybe.Nothing;
        }
    }

    return elm.Native.Json.values = {
        toString   : F2(toString),
        fromString : fromString,
        fromJS     : fromJS,
        toJS       : toJS
    };

};
