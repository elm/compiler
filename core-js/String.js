var String = function() {
  var properEscape = function(str) {
      str.replace('"', "&#34;");
      str.replace("&", "&#38;");
      str.replace("'", "&#39;");
      str.replace("<", "&#60;");
      str.replace(">", "&#62;");
      return str;
  };

  var toText = function(elmList) {
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return String.properEscape(a.join(''));
  };

  return {toText : toText,
	  properEscape : properEscape };
}();