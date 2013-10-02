Elm.Native.String = {};
Elm.Native.String.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.String = elm.Native.String || {};
    if (elm.Native.String.values) return elm.Native.String.values;
    if ('values' in Elm.Native.String)
        return elm.Native.String.values = Elm.Native.String.values;

    function isEmpty(str) {
        return str.length === 0;
    }
    function map(f,str) {
        return str.map(f);
    }
    function filter(pred,str) {
        return str.filter(pred);
    }
    function reverse(str) {
        return str.reverse();
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

    function strip(str) {
        return str.trim();
    }
    function stripLeft(str) {
        return str.trimLeft();
    }
    function stripRight(str) {
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

    return {
        isEmpty: isEmpty,
        map: F2(map),
        filter: F2(filter),
        reverse: reverse,

        repeat: F2(repeat),
        center: F3(center),
        justifyLeft: F3(justifyLeft),
        justifyRight: F3(justifyRight),

        strip: strip,
        stripLeft: stripLeft,
        stripRight: stripRight,

        words: words,
        unwords: unwords,
        lines: lines,
        unlines: unlines,

    };
};