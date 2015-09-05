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
   var Cons = F2(function (a,b) {
      return {ctor: "Cons"
             ,_0: a
             ,_1: b};
   });
   var Nil = {ctor: "Nil"};
   var zip = F2(function (list1,
   list2) {
      var _v0 = {ctor: "_Tuple2"
                ,_0: list1
                ,_1: list2};
      switch (_v0.ctor)
      {case "_Tuple2":
         switch (_v0._0.ctor)
           {case "Nil": return Nil;}
           switch (_v0._1.ctor)
           {case "Nil": return Nil;}
           switch (_v0._0.ctor)
           {case "Cons":
              switch (_v0._1.ctor)
                {case "Cons": return A2(Cons,
                     {ctor: "_Tuple2"
                     ,_0: _v0._0._0
                     ,_1: _v0._1._0},
                     A2(zip,_v0._0._1,_v0._1._1));}
                break;}
           break;}
      _U.badCase($moduleName,
      "bugs in reporting the exact location right now");
   });
   _elm.Main.values = {_op: _op
                      ,Nil: Nil
                      ,Cons: Cons
                      ,zip: zip};
   return _elm.Main.values;
};