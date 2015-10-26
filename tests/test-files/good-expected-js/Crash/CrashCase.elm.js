Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Debug = Elm.Debug.make(_elm);
   var _op = {};
   var test = function (x) {
      var _p0 = x;
      if (_p0 === 1) {
            return 2;
         } else {
            return _U.crashCase("Main",
            {start: {line: 5,column: 5},end: {line: 9,column: 43}},
            _p0)("unexpected value");
         }
   };
   return _elm.Main.values = {_op: _op,test: test};
};