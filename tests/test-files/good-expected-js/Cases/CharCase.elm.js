Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var foo = function (x) {
      var _p0 = x;
      if (_p0.valueOf() === "A") {
            return "hello";
         } else {
            return "letter A was not seen";
         }
   };
   return _elm.Main.values = {_op: _op,foo: foo};
};