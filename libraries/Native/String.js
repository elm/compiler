Elm.Native.String = {};
Elm.Native.String.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.String = elm.Native.String || {};
    if (elm.Native.String.values) return elm.Native.String.values;
    if ('values' in Elm.Native.String)
        return elm.Native.String.values = Elm.Native.String.values;

    var Maybe = Elm.Maybe.make(elm);
    var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

    function isEmpty(str) {
        return str.length === 0;
    }
    function cons(chr,str) {
        return chr + str;
    }
    function uncons(str) {
        var chr;
        return (chr = str[0]) ? Maybe.Just(Tuple2(chr, str.slice(1)))
                              : Maybe.Nothing;
    }
    function length(str) {
        return str.length;
    }
    function map(f,str) {
        return str.map(f);
    }
    function filter(pred,str) {
        return str.filter(pred);
    }
    function reverse(str) {
        return str.split('').reverse().join('');
    }
    function foldl(f,b,str) {
        var len = str.length;
        for (var i = 0; i < len; ++i) {
            b = A2(f, str[i], b);
        }
        return b;
    }
    function foldr(f,b,str) {
        for (var i = str.length; i--; ) {
            b = A2(f, str[i], b);
        }
        return b;
    }

    function split(sep, str) {
        return str.split(sep);
    }
    function join(sep, strs) {
        return strs.join(sep);
    }
    function repeat(n, chr) {
        var result = '';
        while (n > 0) {
            if (n & 1) result += chr;
            n >>= 1, chr += chr;
        }
        return result;
    }
    function center(n,chr,str) {
        var half = n - str.length / 2;
        return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
    }
    function justifyLeft(n,chr,str) {
        return str + repeat(n - str.length, chr);
    }
    function justifyRight(n,chr,str) {
        return repeat(n - str.length, chr) + str;
    }

    function trim(str) {
        return str.trim();
    }
    function trimLeft(str) {
        return str.trimLeft();
    }
    function trimRight(str) {
        return str.trimRight();
    }

    function words(str) {
        return str.split(/\s+/g);
    }
    function unwords(str) {
        return str.join(' ');
    }
    function lines(str) {
        return str.split(/\r\n|\r|\n/g);
    }
    function unlines(str) {
        return str.join('\n');
    }

    function toUpper(str) {
        return str.toUpperCase();
    }
    function toLower(str) {
        return str.toLowerCase();
    }

    function any(pred, str) {
        for (var i = str.length; i--; ) {
            if (pred(str[i])) return true;
        }
        return false;
    }
    function all(pred, str) {
        for (var i = str.length; i--; ) {
            if (!pred(str[i])) return false;
        }
        return true;
    }

    return Elm.Native.String.values = {
        isEmpty: isEmpty,
        cons: F2(cons),
        uncons: uncons,
        length: length,
        map: F2(map),
        filter: F2(filter),
        reverse: reverse,
        foldl: F3(foldl),
        foldr: F3(foldr),

        split: F2(split),
        join: F2(join),
        repeat: F2(repeat),
        center: F3(center),
        justifyLeft: F3(justifyLeft),
        justifyRight: F3(justifyRight),

        trim: trim,
        trimLeft: trimLeft,
        trimRight: trimRight,

        words: words,
        unwords: unwords,
        lines: lines,
        unlines: unlines,

        toUpper: toUpper,
        toLower: toLower,

        any: F2(any),
        all: F2(all)
    };
};