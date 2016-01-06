Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var go = function (n$) {
      go: while (true) if (_U.cmp(n$,0) > 0) {
            var _v0 = n$ - 1;
            n$ = _v0;
            continue go;
         } else return 1;
   };
   return _elm.Main.values = {_op: _op,go: go};
};
