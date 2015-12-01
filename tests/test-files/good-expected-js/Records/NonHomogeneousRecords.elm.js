Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var bindFields = function (thing) {    var y = thing.y;var x = thing.x;return thing;};
   var Thing = F2(function (a,b) {    return {x: a,y: b};});
   return _elm.Main.values = {_op: _op,Thing: Thing,bindFields: bindFields};
};