Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main";
   var extractVec = function (v) {
      return {_: {},x: v.x,y: v.y};
   };
   _elm.Main.values = {_op: _op
                      ,Vec2Ext: Vec2Ext
                      ,extractVec: extractVec};
   return _elm.Main.values;
};