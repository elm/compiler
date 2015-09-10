var elm_lang$core$Main$_op = {};
var elm_lang$core$Main$pairs = Elm.Native.Port.make(_elm).inbound("pairs",
"Main.Point String String",
function (v) {
   return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                          ,x: typeof v.x === "string" || typeof v.x === "object" && v.x instanceof String ? v.x : _U.badPort("a string",
                                                          v.x)
                                                          ,y: typeof v.y === "string" || typeof v.y === "object" && v.y instanceof String ? v.y : _U.badPort("a string",
                                                          v.y)} : _U.badPort("an object with fields `x`, `y`",
   v);
});
var elm_lang$core$Main$points = Elm.Native.Port.make(_elm).inbound("points",
"Main.Point2D",
function (v) {
   return typeof v === "object" && "x" in v && "y" in v ? {_: {}
                                                          ,x: typeof v.x === "number" ? v.x : _U.badPort("a number",
                                                          v.x)
                                                          ,y: typeof v.y === "number" ? v.y : _U.badPort("a number",
                                                          v.y)} : _U.badPort("an object with fields `x`, `y`",
   v);
});
var elm_lang$core$Main$Point = F2(function (a,
b) {
   return {_: {},x: a,y: b};
});