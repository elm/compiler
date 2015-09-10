var elm_lang$core$Main$_op = {};
var elm_lang$core$Main$any = F2(function (f,l) {
   any: while (true) {
      var _p0 = l;
      switch (_p0.ctor)
      {case "::": var x = _p0._0;
           var xs = _p0._1;
           if (f(x)) return true; else {
                 var _v0 = f,_v1 = xs;
                 f = _v0;
                 l = _v1;
                 continue any;
              }
         default: return false;}
   }
});