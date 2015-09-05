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
   var myId = function (x) {
      var y = x;
      return y;
   };
   _elm.Main.values = {_op: _op
                      ,myId: myId};
   return _elm.Main.values;
};