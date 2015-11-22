Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),$Debug = Elm.Debug.make(_elm);
   var _op = {};
   var test = function (x) {    return x ? 2 : _U.crash("Main",{start: {line: 8,column: 9},end: {line: 8,column: 20}})("unexpected value");};
   return _elm.Main.values = {_op: _op,test: test};
};