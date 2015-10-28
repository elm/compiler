Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var always = F2(function (x,s) {    return x;});
   _op["|>"] = F2(function (a,f) {    return f(a);});
   var value = A2(_op["|>"],"Hi",always("Hello"));
   return _elm.Main.values = {_op: _op
                             ,always: always
                             ,value: value};
};