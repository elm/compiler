Elm.Otherwise = Elm.Otherwise || {};
Elm.Otherwise.make = function (_elm) {
   "use strict";
   _elm.Otherwise = _elm.Otherwise || {};
   if (_elm.Otherwise.values)
   return _elm.Otherwise.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Otherwise";
   var boo = false ? "Yay" : "Boo";
   var otherwise = 3;
   _elm.Otherwise.values = {_op: _op
                           ,otherwise: otherwise
                           ,boo: boo};
   return _elm.Otherwise.values;
};