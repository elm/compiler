
var Foreign = function() {
  var JavaScript = function() {
    function castJSBoolToBool(b) { return b; }
    function castBoolToJSBool(b) { return b; }

    function castJSNumberToFloat(n) { return n; }
    function castFloatToJSNumber(n) { return n; }

    function castJSNumberToInt(n) { return ~~n; }
    function castIntToJSNumber(n) { return n; }

    var castJSElementToElement = Element.jsElement;
    function castElementToJSElement(elem) { return elem; }

    function castJSArrayToList(arr) {
	var list = ["Nil"];
	for (var i = arr.length; i--; ) {
	    list = [ "Cons", arr[i], list ];
	}
	return list;
    }
    function castListToJSArray(list) {
	var a = [];
	while (list[0] === "Cons") {
	    a.push(list[1]);
	    list = list[2];
	}
	return a;
    }

    var castJSStringToString = castJSArrayToList
    function castStringToJSString(str) {
	if (typeof str === "string") return str;
	return castListToJSArray(str).join('');
    }

    return {castJSBoolToBool:castJSBoolToBool,
	    castBoolToJSBool:castBoolToJSBool,
	    castJSNumberToFloat:castJSNumberToFloat,
	    castFloatToJSNumber:castFloatToJSNumber,
	    castJSNumberToInt:castJSNumberToInt,
	    castIntToJSNumber:castIntToJSNumber,
	    castJSElementToElement:castJSElementToElement,
	    castElementToJSElement:castElementToJSElement,
	    castJSArrayToList:castJSArrayToList,
	    castListToJSArray:castListToJSArray,
	    castJSStringToString:castJSStringToString,
	    castStringToJSString:castStringToJSString
    };
  }();
  return {JavaScript:JavaScript};
}();