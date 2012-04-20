
var Text = function() {
  var fromString = function(elmList) {
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return String.properEscape(a.join(''));
  };

  var addTag = function(tag) { return function(text) {
	return '<' + tag + '>' + text + '</' + tag + '>';
    };
  };
  var addStyle = function(style, value) { return function(text) {
	return "<span style='" + style + ":" + value + "'>" + text + "</span>";
    };
  };

  var typeface = function(name) { return addStyle('font-family', name); };
  var size = function(px) {
    return addStyle('font-size', px + 'px');
  };
  var header = addTag('h1');
  var height = function(h) { return addStyle('font-size', h+'em'); }
  var italic = addStyle('font-style', 'italic');
  var bold = addTag('b');
  var color = function(c) {
    return addStyle('color', Color.Internal.extract(c));
  };
  var underline = addStyle('text-decoration', 'underline');
  var overline = addStyle('text-decoration', 'overline');
  var strikeThrough = addStyle('text-decoration', 'line-through');
  var link = function(href) { return function(text) {
      return "<a href='" + fromString(href) + "'>" + text + "</a>";
    };
  };

  return {fromString : fromString,
	  toText: fromString,
	  header : header,
	  height : height,
	  italic : italic,
	  bold : bold,
	  underline : underline,
	  overline : overline,
	  strikeThrough : strikeThrough,
	  monospace : typeface('monospace'),
	  color : color,
	  link : link };
}();