Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var commentStart = "{-";
   var singleQuote = _U.chr("\'");
   var doubleQuote = _U.chr("\"");
   var variablesWithPrimes = function (x) {    var x$ = x;return x$;};
   return _elm.Main.values = {_op: _op,variablesWithPrimes: variablesWithPrimes,doubleQuote: doubleQuote,singleQuote: singleQuote,commentStart: commentStart};
};