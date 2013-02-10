function nativeColor(elm) {

function extract(c) {
    if (c[4] === 1) { return 'rgb(' + c[1] + ',' + c[2] + ',' + c[3] + ')'; }
    return 'rgba(' + c[1] + ',' + c[2] + ',' + c[3] + ',' + c[4] + ')';
}
function complement(rgb) {
  var hsv = toHSV(rgb);
  hsv.hue = (hsv.hue + 180) % 360;
  return toRGB(hsv);
}

function hsva(h) { return function(s) { return function(v) { return function(a) {
    var clr = toRGB({hue:h, saturation:s, value:v});
    clr[4] = a;
    return clr;
  }; }; };
}

function hsv(h) { return function(s) { return function(v) {
   return toRGB({hue:h, saturation:s, value:v}); }; }; }

function toHSV(rgb) {
    var hsv = {};
    var r = rgb[1] / 255.0, g = rgb[2] / 255.0, b = rgb[3] / 255.0;
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
    return [ "Color", norm(r+m), norm(g+m), norm(b+m), 1 ];
}

elm.Native.Color = {
    hsva:hsva,
    hsv:hsv,
    complement:complement,
    extract:extract};
}