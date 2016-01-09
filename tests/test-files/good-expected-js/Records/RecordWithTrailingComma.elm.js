Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var multiple = {one: 1,two: "2",three: false};
   var multipleUpdate = _U.update(multiple,{two: "x",three: true});
   var single = {one: 1};
   var singleUpdate = _U.update(single,{one: 2});
   return _elm.Main.values = {_op: _op,single: single,singleUpdate: singleUpdate,multiple: multiple,multipleUpdate: multipleUpdate};
};