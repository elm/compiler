Elm.Native.Error = {};
Elm.Native.Error.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Error = elm.Native.Error || {};
    if (elm.Native.Error.values) return elm.Native.Error.values;

    function indent(lines) {
        var msg = '';
        for (var i = 0; i < lines.length; ++i) {
            msg += '<br/>&nbsp; &nbsp; ' + lines[i];
        }
        return msg;
    }

    function Case(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in case-expression.',
                          'Make sure your patterns cover every case!']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function If(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in multi-way-if expression.',
                          'It is best to use \'otherwise\' as the last branch of multi-way-if.']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function raise(str) { throw new Error(str); }

    return elm.Native.Error.values = { Case: Case, If: If, raise: raise };
};
