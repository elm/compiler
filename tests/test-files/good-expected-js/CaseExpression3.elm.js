var elm_lang$core$Main$_op = {};
var elm_lang$core$Main$Cons = F2(function (a,
b) {
   return {ctor: "Cons",_0: a,_1: b};
});
var elm_lang$core$Main$Nil = {ctor: "Nil"};
var elm_lang$core$Main$zip = F2(function (list1,
list2) {
   var _p0 = {ctor: "_Tuple2"
             ,_0: list1
             ,_1: list2};
   switch (_p0._0.ctor)
   {case "Nil": return elm_lang$core$Main$Nil;
      default: switch (_p0._1.ctor)
        {case "Nil": return elm_lang$core$Main$Nil;
           default: var x = _p0._0._0;
             var xs = _p0._0._1;
             var y = _p0._1._0;
             var ys = _p0._1._1;
             return A2(elm_lang$core$Main$Cons,
             {ctor: "_Tuple2",_0: x,_1: y},
             A2(elm_lang$core$Main$zip,xs,ys));}}
});