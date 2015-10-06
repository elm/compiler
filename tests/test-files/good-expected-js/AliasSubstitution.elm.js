Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var extractVec = function (v) {    return {x: v.x,y: v.y};};
   return _elm.Main.values = {_op: _op,extractVec: extractVec};
};