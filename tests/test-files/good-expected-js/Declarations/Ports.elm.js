Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var students = Elm.Native.Port.make(_elm).outbound("students",
   function (v) {
      return Elm.Native.List.make(_elm).toArray(v).map(function (v) {    return {name: v.name,age: v.age};});
   },
   _U.list([{name: "Tom",age: 42}]));
   var time = Elm.Native.Port.make(_elm).outbound("time",function (v) {    return v;},3.14);
   var fortyTwo = Elm.Native.Port.make(_elm).outbound("fortyTwo",function (v) {    return v;},42);
   var record = Elm.Native.Port.make(_elm).inbound("record",
   "{ x : Float, y : Float }",
   function (v) {
      return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                             ,x: typeof v.x === "number" ? v.x : _U.badPort("a number",v.x)
                                                             ,y: typeof v.y === "number" ? v.y : _U.badPort("a number",
                                                             v.y)} : _U.badPort("an object with fields `x`, `y`",v);
   });
   var array = Elm.Native.Port.make(_elm).inbound("array",
   "List Float",
   function (v) {
      return typeof v === "object" && v instanceof Array ? Elm.Native.List.make(_elm).fromArray(v.map(function (v) {
         return typeof v === "number" ? v : _U.badPort("a number",v);
      })) : _U.badPort("an array",v);
   });
   var tuple = Elm.Native.Port.make(_elm).inbound("tuple",
   "( Float, Bool )",
   function (v) {
      return typeof v === "object" && v instanceof Array ? {ctor: "_Tuple2"
                                                           ,_0: typeof v[0] === "number" ? v[0] : _U.badPort("a number",v[0])
                                                           ,_1: typeof v[1] === "boolean" ? v[1] : _U.badPort("a boolean (true or false)",
                                                           v[1])} : _U.badPort("an array",v);
   });
   var number = Elm.Native.Port.make(_elm).inbound("number",
   "Int",
   function (v) {
      return typeof v === "number" && isFinite(v) && Math.floor(v) === v ? v : _U.badPort("an integer",v);
   });
   var userID = Elm.Native.Port.make(_elm).inbound("userID",
   "String",
   function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _U.badPort("a string",v);
   });
   return _elm.Main.values = {_op: _op};
};