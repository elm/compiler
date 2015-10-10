Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var toName = function (a) {
      var name = function () {
         var _p0 = a;
         if (_p0.ctor === "First") {
               return "True";
            } else {
               return _p0._0;
            }
      }();
      return name;
   };
   var Second = F2(function (a,b) {
      return {ctor: "Second",_0: a,_1: b};
   });
   var First = {ctor: "First"};
   return _elm.Main.values = {_op: _op
                             ,First: First
                             ,Second: Second
                             ,toName: toName};
};