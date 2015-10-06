Elm.Otherwise = Elm.Otherwise || {};
Elm.Otherwise.make = function (_elm) {
   "use strict";
   _elm.Otherwise = _elm.Otherwise || {};
   if (_elm.Otherwise.values) return _elm.Otherwise.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var boo = false ? "Yay" : "Boo";
   var otherwise = 3;
   return _elm.Otherwise.values = {_op: _op
                                  ,otherwise: otherwise
                                  ,boo: boo};
};