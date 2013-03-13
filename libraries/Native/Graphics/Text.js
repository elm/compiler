
Elm.Native.Graphics.Text = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  elm.Native.Graphics = elm.Native.Graphics || {};
  if (elm.Native.Graphics.Text) return elm.Native.Graphics.Text;

  var JS = Elm.JavaScript(elm);
  var htmlHeight = Elm.Native.Utils(elm).htmlHeight;
  var Color = Elm.Native.Graphics.Color(elm);
  var Element = Elm.Graphics.Element(elm);

  function makeSpaces(s) {
    if (s.length == 0) { return s; }
    var arr = s.split('');
    if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
    for (var i = arr.length; --i; ) {
      if (arr[i][0] == ' ' && arr[i-1] == ' ') {
        arr[i-1] = arr[i-1] + arr[i];
        arr[i] = '';
      }
    }
    for (var i = arr.length; i--; ) {
      if (arr[i].length > 1 && arr[i][0] == ' ') {
        var spaces = arr[i].split('');
        for (var j = spaces.length - 2; j >= 0; j -= 2) {
          spaces[j] = '&nbsp;';
        }
        arr[i] = spaces.join('');
      }
    }
    arr = arr.join('');
    if (arr[arr.length-1] === " ") {
	return arr.slice(0,-1) + '&nbsp;';
    }
    return arr;
  }

  function properEscape(str) {
    if (str.length == 0) return str;
    str = str //.replace(/&/g,  "&#38;")
	.replace(/"/g, /*"*/ '&#34;')
	.replace(/'/g, /*'*/ "&#39;")
	.replace(/</g,  "&#60;")
	.replace(/>/g,  "&#62;")
	.replace(/\n/g, "<br/>");
    var arr = str.split('<br/>');
    for (var i = arr.length; i--; ) {
	arr[i] = makeSpaces(arr[i]);
    }
    return arr.join('<br/>');
  }

  function toText(str) { return properEscape(JS.fromString(str)); }

  function addTag(tag) { return function(text) {
      return '<' + tag + ' style="padding:0;margin:0">' + text + '</' + tag + '>';
    }
  }
  
  function addStyle(style, value, text) {
    return "<span style='" + style + ":" + value + "'>" + text + "</span>";
  }

  function typeface(name, text) {
    return addStyle('font-family', JS.fromString(name), text);
  }
  function size(px, text) { return addStyle('font-size', px + 'px', text) }
  var header = addTag('h1');
  function height(h, text) { return addStyle('font-size', hf+'em', text) }
  function italic(text) { return addStyle('font-style', 'italic', text) }
  var bold = addTag('b');
  function color(c, text) {
    return addStyle('color', Color.extract(c), text);
  }
  function underline(text) { return addStyle('text-decoration', 'underline', text) }
  function overline(text) { return addStyle('text-decoration', 'overline', text) }
  function strikeThrough(text) {
      return addStyle('text-decoration', 'line-through', text);
  }
  function link(href, text) {
    return "<a href='" + toText(href) + "'>" + text + "</a>";
  }

  function position(pos) { return function(text) {
    var e = {ctor:'RawHtml',
	     _0: '<div style="padding:0;margin:0;text-align:' +
                   pos + '">' + text + '</div>'
            };
    var w = elm.node.clientWidth;
    return A3(Element.newElement, w, A2(htmlHeight, w, text), e);
   }
  }

  function asText(v) {
    return position('left')(typeface('monospace', toText(show(v))));
  }

  return elm.Native.Graphics.Text = {
      toText: toText,

      header : header,
      height : F2(height),
      italic : italic,
      bold : bold,
      underline : underline,
      overline : overline,
      strikeThrough : strikeThrough,
      monospace : function(text) { return typeface('monospace', text) },
      typeface : F2(typeface),
      color : F2(color),
      link : F2(link),

      justified : position('justify'),
      centered : position('center'),
      righted : position('right'),
      text : position('left'),

      asText : asText
  };

};