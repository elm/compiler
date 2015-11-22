Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var Cons = F2(function (a,b) {    return {ctor: "Cons",_0: a,_1: b};});
   var Nil = {ctor: "Nil"};
   var zip = F2(function (list1,list2) {
      var _p0 = {ctor: "_Tuple2",_0: list1,_1: list2};
      if (_p0._0.ctor === "Nil") {
            return Nil;
         } else {
            if (_p0._1.ctor === "Nil") {
                  return Nil;
               } else {
                  return A2(Cons,{ctor: "_Tuple2",_0: _p0._0._0,_1: _p0._1._0},A2(zip,_p0._0._1,_p0._1._1));
               }
         }
   });
   return _elm.Main.values = {_op: _op,Nil: Nil,Cons: Cons,zip: zip};
};