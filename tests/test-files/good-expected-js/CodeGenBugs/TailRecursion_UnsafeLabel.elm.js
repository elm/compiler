Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var foldl$ = F3(function (update,state,items) {
      foldl$: while (true) {
         var _p0 = items;
         if (_p0.ctor === "Nil") {
               return state;
            } else {
               var _v1 = update,_v2 = A2(update,_p0._0,state),_v3 = _p0._1;
               update = _v1;
               state = _v2;
               items = _v3;
               continue foldl$;
            }
      }
   });
   var Cons = F2(function (a,b) {
      return {ctor: "Cons",_0: a,_1: b};
   });
   var Nil = {ctor: "Nil"};
   return _elm.Main.values = {_op: _op
                             ,Nil: Nil
                             ,Cons: Cons
                             ,foldl$: foldl$};
};