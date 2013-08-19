
Elm.Native.Char = function(elm) {
 'use strict';

 elm.Native = elm.Native || {};
 if (elm.Native.Char) return elm.Native.Char;

 function isBetween(lo,hi) { return function(chr) {
	 var c = chr.charCodeAt(0);
	 return lo <= c && c <= hi;
     };
 }
 var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
 var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
 var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));

 return elm.Native.Char = {
     fromCode : function(c) { return String.fromCharCode(c); },
     toCode   : function(c) { return c.toUpperCase().charCodeAt(0); },
     toUpper  : function(c) { return c.toUpperCase(); },
     toLower  : function(c) { return c.toLowerCase(); },
     toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
     toLocaleLower : function(c) { return c.toLocaleLowerCase(); },
     isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
     isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
     isDigit    : isDigit,
     isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
     isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
 };

};
