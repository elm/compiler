Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _op = {};
   var s = "\nhere\'s a quote: \"\n\n";
   return _elm.Main.values = {_op: _op,s: s};
};