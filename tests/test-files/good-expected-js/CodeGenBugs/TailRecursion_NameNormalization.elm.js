Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var go = function (n$) {    go: while (true) if (n$) {    var _v0 = n$;n$ = _v0;continue go;} else return false;};
   return _elm.Main.values = {_op: _op,go: go};
};