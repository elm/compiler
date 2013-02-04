/*! Char !*/

/*[Classification]*/

/** isUpper : Char -> Bool
    Selects upper case letters.
**/
/** isLower : Char -> Bool
    Selects lower case letters.
**/
/** isDigit : Char -> Bool
    Selects ASCII digits (0..9).
**/
/** isOctDigit : Char -> Bool
    Selects ASCII octal digits (0..7).
**/
/** isHexDigit : Char -> Bool
    Selects ASCII hexadecimal digits (0..9a..fA..F).
**/

/*[Conversion]*/

/** toUpper : Char -> Char
    Convert to upper case.
**/
/** toLower : Char -> Char
    Convert to lower case.
**/
/** toLocaleUpper : Char -> Char
    Convert to upper case, according to any locale-specific case mappings.
**/
/** toLocaleLower : Char -> Char
    Convert to lower case, according to any locale-specific case mappings.
**/
/** toCode : Char -> Int
    Convert to unicode.
**/
/** fromCode : Int -> Char
    Convert from unicode.
**/

Elm.Char = function() {
    function isBetween(lo,hi) { return function(chr) {
	    var c = chr.charCodeAt(0);
	    return lo <= c && c <= hi;
	};
    }
    var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
    var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
    var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));
    
    return {fromCode : function(c) { return String.fromCharCode(c); },
	    toCode : function(c) { return c.charCodeAt(0); },
	    toUpper : function(c) { return c.toUpperCase(); },
	    toLower : function(c) { return c.toLowerCase(); },
	    toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
	    toLocaleLower : function(c) { return c.toLocaleLowerCase(); },
	    isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
	    isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
	    isDigit    : isDigit,
	    isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
	    isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
    };
}();
