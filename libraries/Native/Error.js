
Elm.Native.Error = function(elm) {
    'use strict';
    elm.Native = elm.Native || {};
    if (elm.Native.Error) return elm.Native.Error;

    var fromString = Elm.Native.JavaScript(elm).fromString;

    function Case(moduleName, span) { 
	var msg = 'Non-exhaustive pattern match in case expression in module '
	throw new Error(msg + moduleName + " (" + span + ")")
    }

    function If(moduleName, span) { 
	var msg = 'Non-exhaustive pattern match in multi-way-if expression in module '
	throw new Error(msg + moduleName + " (" + span + ")")
    }

    function raise(str) { throw new Error(fromString(str)); }

    return elm.Native.Error = { Case: Case, If: If, raise: raise };
};