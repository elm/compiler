Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var apply = function (f) {    var g = function (x) {    return f(x);};return g;};
   return _elm.Main.values = {_op: _op,apply: apply};
};