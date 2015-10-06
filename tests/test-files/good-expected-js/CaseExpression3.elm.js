Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var Cons = F2(function (a,b) {
      return {ctor: "Cons",_0: a,_1: b};
   });
   var Nil = {ctor: "Nil"};
   var zip = F2(function (list1,list2) {
      var _p0 = {ctor: "_Tuple2",_0: list1,_1: list2};
      if (_p0._0.ctor === "Nil") {
            return Nil;
         } else {
            if (_p0._1.ctor === "Nil") {
                  return Nil;
               } else {
                  var x = _p0._0._0,
                  xs = _p0._0._1,
                  y = _p0._1._0,
                  ys = _p0._1._1;
                  return A2(Cons,{ctor: "_Tuple2",_0: x,_1: y},A2(zip,xs,ys));
               }
         }
   });
   return _elm.Main.values = {_op: _op
                             ,Nil: Nil
                             ,Cons: Cons
                             ,zip: zip};
};