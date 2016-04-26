Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm),$Foobar = Elm.Foobar.make(_elm);
   var _op = {};
   var count = function (action) {    var _p0 = action;if (_p0._0.ctor === "Foo") {    return 0;} else {    return 1;}};
   var Action = function (a) {    return {ctor: "Action",_0: a};};
   return _elm.Main.values = {_op: _op,Action: Action,count: count};
};