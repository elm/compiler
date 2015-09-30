Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _op = {};
   var commentStart = "{-";
   var singleQuote = _utils.chr("\'");
   var doubleQuote = _utils.chr("\"");
   var variablesWithPrimes = function (x) {
      var x$ = x;
      return x$;
   };
   return _elm.Main.values = {_op: _op
                             ,variablesWithPrimes: variablesWithPrimes
                             ,doubleQuote: doubleQuote
                             ,singleQuote: singleQuote
                             ,commentStart: commentStart};
};