
Elm.Error = {
    Case : function(span) { 
	var msg = 'Non-exhaustive pattern match in case expression'
	throw new Error(msg + " (" + span + ")")
    }
    If : function(span) { 
	var msg = 'Non-exhaustive pattern match in multi-way-if expression'
	throw new Error(msg + " (" + span + ")")
    }
};