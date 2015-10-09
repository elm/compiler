Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var Nothing = {ctor: "Nothing"};
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   var withDefault0 = function (maybe) {
      var _p0 = maybe;
      if (_p0.ctor === "Just") {
            var maybe = Just("shadow argument, but do not overwrite it in JS!");
            return _p0._0;
         } else {
            return 0;
         }
   };
   return _elm.Main.values = {_op: _op
                             ,Just: Just
                             ,Nothing: Nothing
                             ,withDefault0: withDefault0};
};