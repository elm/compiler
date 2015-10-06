Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var any = F2(function (f,l) {
      any: while (true) {
         var _p0 = l;
         if (_p0.ctor === "::") {
               var x = _p0._0,xs = _p0._1;
               if (f(x)) return true; else {
                     var _v0 = f,_v1 = xs;
                     f = _v0;
                     l = _v1;
                     continue any;
                  }
            } else {
               return false;
            }
      }
   });
   return _elm.Main.values = {_op: _op,any: any};
};