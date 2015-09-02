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
   var any = F2(function (f,l) {
      _v0: while (true) {
         switch (l.ctor)
         {case "::": if (f(l._0))
              return true; else {
                    var _v4 = f,_v5 = l._1;
                    f = _v4;
                    l = _v5;
                    continue _v0;
                 }}
         return false;
      }
   });
   _elm.Main.values = {_op: _op
                      ,any: any};
   return _elm.Main.values;
};