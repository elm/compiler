Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values) return _elm.Main.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var pairs = Elm.Native.Port.make(_elm).inbound("pairs",
   "Main.Point String String",
   function (v) {
      return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                             ,x: typeof v.x === "string" || typeof v.x === "object" && v.x instanceof String ? v.x : _U.badPort("a string",
                                                             v.x)
                                                             ,y: typeof v.y === "string" || typeof v.y === "object" && v.y instanceof String ? v.y : _U.badPort("a string",
                                                             v.y)} : _U.badPort("an object with fields `x`, `y`",v);
   });
   var points = Elm.Native.Port.make(_elm).inbound("points",
   "Main.Point2D",
   function (v) {
      return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                             ,x: typeof v.x === "number" ? v.x : _U.badPort("a number",v.x)
                                                             ,y: typeof v.y === "number" ? v.y : _U.badPort("a number",
                                                             v.y)} : _U.badPort("an object with fields `x`, `y`",v);
   });
   var Point = F2(function (a,b) {    return {x: a,y: b};});
   return _elm.Main.values = {_op: _op,Point: Point};
};