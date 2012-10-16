
var Elm = Elm || {};
Elm.JavaScript = function() {
  function castJSBoolToBool(b) { return b; }
  function castBoolToJSBool(b) { return b; }

  function castJSNumberToFloat(n) { return n; }
  function castFloatToJSNumber(n) { return n; }
  
  function castJSNumberToInt(n) { return ~~n; }
  function castIntToJSNumber(n) { return n; }

  function castJSElementToElement(w) {
    return function(h) {
      return function(node) {
	return ["Element",Guid.guid(),
		["EExternalHtml",node],
		w,h,1,Nothing,Nothing];
      }
    }
  }
  function castElementToJSElement(elem) { return Render.render(elem); }

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
  
  var castJSStringToString = castJSArrayToList;
  function castStringToJSString(str) {
      if (typeof str === "string") return str;
      return castListToJSArray(str).join('');
  }
  
  function fromTuple(t) { return t.slice(1); }
  function toTuple(a) { return ["Tuple" + a.length].concat(a); }
  
  return {castJSBoolToBool:castJSBoolToBool,
	  castBoolToJSBool:castBoolToJSBool,
	  castJSNumberToFloat:castJSNumberToFloat,
	  castFloatToJSNumber:castFloatToJSNumber,
	  castJSNumberToInt:castJSNumberToInt,
	  castIntToJSNumber:castIntToJSNumber,
	  Experimental : {castJSElementToElement:castJSElementToElement,
			  castElementToJSElement:castElementToJSElement},
	  castJSArrayToList:castJSArrayToList,
	  castListToJSArray:castListToJSArray,
	  castJSStringToString:castJSStringToString,
	  castStringToJSString:castStringToJSString,
	  castTupleToJSTuple2:fromTuple,
	  castTupleToJSTuple3:fromTuple,
	  castTupleToJSTuple4:fromTuple,
	  castTupleToJSTuple5:fromTuple,
	  castJSTupleToTuple2:toTuple,
	  castJSTupleToTuple3:toTuple,
	  castJSTupleToTuple4:toTuple,
	  castJSTupleToTuple5:toTuple
  };
}();
