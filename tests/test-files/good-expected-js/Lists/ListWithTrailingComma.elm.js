Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var multiple = _U.list([{ctor: "_Tuple2",_0: 1,_1: 2},{ctor: "_Tuple2",_0: 3,_1: 4}]);
   var single = _U.list([{ctor: "_Tuple2",_0: 1,_1: 2}]);
   return _elm.Main.values = {_op: _op,single: single,multiple: multiple};
};