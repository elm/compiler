var Elm = Elm || {};
Elm.Color = function() {
  function Color_0(a1) {
    return function(a2) {
      return function(a3) {
        return function(a4) {
          return["Color", a1, a2, a3, a4]
        }
      }
    }
  }
  var rgba_1 = Color_0;
  var red_3 = ["Color",255,0,0,1];
  var green_4 = ["Color",0,255,0,1];
  var blue_5 = ["Color",0,0,255,1];
  var yellow_6 = ["Color",255,255,0,1];
  var cyan_7 = ["Color",0,255,255,1];
  var magenta_8 = ["Color",255,0,255,1];
  var black_9 = ["Color",0,0,0,1];
  var white_10 = ["Color",255,255,255,1];
  var gray_11 = ["Color",128,128,128,1];
  var grey_12 = ["Color",128,128,128,1];
  function rgb_2(r_13) {
    return function(g_14) {
      return function(b_15) {
        return ["Color",r_13,g_14,b_15,1]
      }
    }
  }
  function extract(c) {
      if (c[4] === 1) { return 'rgb(' + c[1] + ',' + c[2] + ',' + c[3] + ')'; }
      return 'rgba(' + c[1] + ',' + c[2] + ',' + c[3] + ',' + c[4] + ')';
  }
function complement(rgb) {
    var hsv = toHSV(rgb);
    hsv.hue = (hsv.hue + 180) % 360;
    return toRGB(hsv);
}

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
    return ["Color", norm(r+m), norm(g+m), norm(b+m), 1 ];
}
  return{rgba:rgba_1, rgb:rgb_2, red:red_3, green:green_4, blue:blue_5, yellow:yellow_6, cyan:cyan_7, magenta:magenta_8, black:black_9, white:white_10, gray:gray_11, grey:grey_12,complement:complement,extract:extract}
}();