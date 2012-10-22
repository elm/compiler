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
  return{rgba:rgba_1, rgb:rgb_2, red:red_3, green:green_4, blue:blue_5, yellow:yellow_6, cyan:cyan_7, magenta:magenta_8, black:black_9, white:white_10, gray:gray_11, grey:grey_12,extract:extract}
}();