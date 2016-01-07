Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var bad = function (arg) {
      return function (x) {
         return x;
      }(function () {    var _p0 = arg;if (_p0.ctor === "Just") {    return true;} else {    var _p1 = true;return _p1 ? false : _p1;}}());
   };
   var Nothing = {ctor: "Nothing"};
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   return _elm.Main.values = {_op: _op,Just: Just,Nothing: Nothing,bad: bad};
};