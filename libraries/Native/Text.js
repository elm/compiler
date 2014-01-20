Elm.Native.Text = {};
Elm.Native.Text.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Text = elm.Native.Text || {};
    if (elm.Native.Text.values) return elm.Native.Text.values;

    var JS = Elm.JavaScript.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Color = Elm.Native.Color.make(elm);
    var Element = Elm.Graphics.Element.make(elm);
    var show = Elm.Native.Show.make(elm).show;

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
	    .replace(/"/g,  '&#34;')
	    .replace(/'/g,  "&#39;")
	    .replace(/</g,  "&#60;")
	    .replace(/>/g,  "&#62;")
	    .replace(/\n/g, "<br/>");
        var arr = str.split('<br/>');
        for (var i = arr.length; i--; ) {
	    arr[i] = makeSpaces(arr[i]);
        }
        return arr.join('<br/>');
    }

    function toText(str) { return Utils.txt(properEscape(JS.fromString(str))); }

    function height(px, text) {
        return { style: 'font-size:' + px + 'px;', text:text }
    }
    function typeface(name, text) {
        return { style: 'font-family:' + name + ';', text:text }
    }
    function monospace(text) {
        return { style: 'font-family:monospace;', text:text }
    }
    function italic(text) {
        return { style: 'font-style:italic;', text:text }
    }
    function bold(text) {
        return { style: 'font-weight:bold;', text:text }
    }
    function link(href, text) {
        return { href: toText(href), text:text };
    }
    function underline(text) {
        return { line: ' underline', text:text };
    }
    function overline(text) {
        return { line: ' overline', text:text };
    }
    function strikeThrough(text) {
        return { line: ' line-through', text:text };
    }

    function color(c, text) {
        var color = (c._3 === 1)
            ? ('rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')')
            : ('rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')');
        return { style: 'color:' + color + ';', text:text };
    }

    function position(align) {
        function create(text) {
            var raw = {
                ctor :'RawHtml',
                html : Utils.makeText(text),
                align: align,
                guid : null,
                args : [],
            };
            var pos = A2(Utils.htmlHeight, 0, raw);
            return A3(Element.newElement, pos._0, pos._1, raw);
        }
        return create;
    }

    function markdown(text, guid) {
        var raw = {
            ctor:'RawHtml',
            html: text,
            align: null,
            guid: guid,
            args: [],
        };
        var pos = A2(Utils.htmlHeight, 0, raw);
        return A3(Element.newElement, pos._0, pos._1, raw);
    }

    var text = position('left');
    function asText(v) {
        return text(monospace(toText(show(v))));
    }

    function plainText(v) {
        return text(toText(v));
    }

    return elm.Native.Text.values = {
        toText: toText,

        height : F2(height),
        italic : italic,
        bold : bold,
        underline : underline,
        overline : overline,
        strikeThrough : strikeThrough,
        monospace : monospace,
        typeface : F2(typeface),
        color : F2(color),
        link : F2(link),

        justified : position('justify'),
        centered : position('center'),
        righted : position('right'),
        text : text,
        plainText : plainText,
        markdown : markdown,

        asText : asText,
    };
};
