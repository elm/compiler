Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var foo = function (_p0) {    var _p1 = _p0;return _p1;};
   return _elm.Main.values = {_op: _op,foo: foo};
};