
Elm.Native.Error = function(elm) {
    'use strict';
    elm.Native = elm.Native || {};
    if (elm.Native.Error) return elm.Native.Error;

    var fromString = Elm.Native.JavaScript(elm).fromString;

    function Case(span) { 
	var msg = 'Non-exhaustive pattern match in case expression'
	throw new Error(msg + " (" + span + ")")
    }

    function If(span) { 
	var msg = 'Non-exhaustive pattern match in multi-way-if expression'
	throw new Error(msg + " (" + span + ")")
    }

    function raise(str) { throw new Error(fromString(str)); }

    return elm.Native.Error = { Case: Case, If: If, raise: raise };
};