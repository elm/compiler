Elm.Native.Color = {};
Elm.Native.Color.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Color = elm.Native.Color || {};
    if (elm.Native.Color.values) return elm.Native.Color.values;

    var Utils = Elm.Native.Utils.make(elm);

    function toCss(c) {
        return (c._3 === 1)
            ? ('rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')')
            : ('rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')');
    }

    function complement(rgb) {
        var hsv = toHSV(rgb);
        hsv.hue = (hsv.hue + 180) % 360;
        return toRGB(hsv);
    }

    function hsva(h,s,v,a) {
        var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
        var clr = toRGB({hue:degree, saturation:s, value:v});
        clr._3 = a;
        return clr;
    }

    function hsv(h,s,v) {
        var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
        return toRGB({hue:degree, saturation:s, value:v});
    }

    function toHSV(rgb) {
        var hsv = {};
        var r = rgb._0 / 255.0, g = rgb._1 / 255.0, b = rgb._2 / 255.0;
        var M = Math.max(r,g,b);
        var m = Math.min(r,g,b);
        var c = M - m;

        var h = 0;
             if (c === 0) { h = 0; }
        else if (M === r) { h = ((g - b) / c) % 6; }
        else if (M === g) { h = ((b - r) / c) + 2; }
        else if (M === b) { h = ((r - g) / c) + 4; }
        h *= 60;

        return { value : M, hue : h, saturation : (M === 0 ? 0 : c / M) };
    }

    function between(lo,hi,x) { return lo <= x && x < hi; }
    function norm(n) { return Math.round(n*255); }

    function toRGB(hsv) {
        var c = hsv.value * hsv.saturation;
        var hue = hsv.hue / 60;
        var x = c * (1 - Math.abs((hue % 2) - 1));
        var r = 0, g = 0, b = 0;
             if (between(0,1,hue)) { r = c; g = x; b = 0; }
        else if (between(1,2,hue)) { r = x; g = c; b = 0; }
        else if (between(2,3,hue)) { r = 0; g = c; b = x; }
        else if (between(3,4,hue)) { r = 0; g = x; b = c; }
        else if (between(4,5,hue)) { r = x; g = 0; b = c; }
        else if (between(5,6,hue)) { r = c; g = 0; b = x; }

        var m = hsv.value - c;
        return { ctor:"Color", _0:norm(r+m), _1:norm(g+m), _2:norm(b+m), _3:1 };
    }

    return elm.Native.Color.values = {
        hsva:F4(hsva),
        hsv:F3(hsv),
        complement:complement,
        toCss:toCss
    };

};
