Elm.Native.Prelude = {};
Elm.Native.Prelude.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Prelude = elm.Native.Prelude || {};
  if (elm.Native.Prelude.values) return elm.Native.Prelude.values;

  var JS = Elm.Native.JavaScript.make(elm);
  var Maybe = Elm.Maybe.make(elm);
  var Char = Elm.Char.make(elm);

  function readInt(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    for (var i = start; i < len; ++i) {
      if (!Char.isDigit(s[i])) { return Maybe.Nothing; }
    }
    return Maybe.Just(parseInt(s, 10));
  }

  function readFloat(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    var dotCount = 0;
    for (var i = start; i < len; ++i) {
      if (Char.isDigit(s[i])) { continue; }
      if (s[i] === '.') {
        dotCount += 1;
        if (dotCount <= 1) { continue; }
      }
      return Maybe.Nothing;
    }
    return Maybe.Just(parseFloat(s));
  }

  return elm.Native.Prelude.values = {
      readInt:readInt,
      readFloat:readFloat
  };

};
