/*
(install node/npm)
npm install buster -g
node test/runtime.js
*/

var buster = require("buster"),
elmExports = require("../dist/data/elm-runtime.js"),
Functions = elmExports.Functions;

var elm = {
  node: {
    addEventListener: function(){}
  },
  inputs: []
};

var N = Elm.Native,
   _N = N.Utils(elm),
   _L = N.List(elm),
   _E = N.Error(elm),
   _str = N.JavaScript(elm).toString;

var Prelude = Elm.Prelude(elm);

buster.testCase("Prelude", {
  "radians are identity": function(){
    assert.equals(Prelude.radians(2), 2);
  },

  "degrees are converted to radians": function(){
    assert.near(Prelude.degrees(42), 0.733, 0.01);
  },

  "turns are converted to radians": function(){
    assert.equals(Prelude.turns(0.5), Math.PI);
  },

  "otherwise is true": function(){
    assert.isTrue(Prelude.otherwise);
  }
});

buster.testCase("Native Prelude", {
  "div": function(){
    assert.equals(Prelude.div.func(1,2), 0);
    assert.equals(Prelude.div.func(2,2), 1);
    assert.equals(Prelude.div.func(2,1), 2);
  },

  "rem": function(){
    assert.equals(Prelude.rem.func(-37,-28),-9);
    assert.equals(Prelude.rem.func(76,4),0);
    assert.equals(Prelude.rem.func(97,-5),2);
  },

  "mod": function(){
    assert.equals(Prelude.mod.func(-37,-28),-9);
    assert.equals(Prelude.mod.func(76,4),0);
    assert.equals(Prelude.mod.func(97,-5),-3);
  },

  "abs": function(){
    assert.equals(Prelude.abs(-1),1);
    assert.equals(Prelude.abs(1),1);
    assert.equals(Prelude.abs(0),0);
  },

  "logBase": function(){
    assert.equals(Prelude.logBase.func(2, 1024), 10);
  },

  "min": function(){
    assert.equals(Prelude.min.func(1,2), 1);
  },

  "max": function(){
    assert.equals(Prelude.max.func(1,2), 2);
  },

  "clamp": function(){
    assert.equals(Prelude.clamp.func(0, 1, 2), 1);
    assert.equals(Prelude.clamp.func(0, 1, -1), 0);
  },

  "xor": function(){
    assert.equals(Prelude.xor.func(false,false), false);
    assert.equals(Prelude.xor.func(false,true), true);
    assert.equals(Prelude.xor.func(true,false), true);
    assert.equals(Prelude.xor.func(true,true), false);
  },

  "not": function(){
    assert.equals(Prelude.not(false), true);
    assert.equals(Prelude.not(true), false);
  },

  "truncate": function(){
    assert.equals(Prelude.truncate(false), 0);
    assert.equals(Prelude.truncate(null), 0);
    assert.equals(Prelude.truncate(undefined), 0);
  },

  "id": function(){
    assert.equals(Prelude.id(1234), 1234);
  },

  "flip": function(){
    var f = Functions.F2(function(a,b){ return [a,b]; });
    assert.equals(Functions.A3(Prelude.flip, f, 1, 2), [2,1]);
  },

  "curry": function(){
    var tuple = {_0: 1, _1: 2, ctor: "Tuple2"},
        f = Functions.F2(function(a,b){ return [a,b]; });
    assert.equals(Functions.A2(Prelude.curry, f, tuple), [1,2]);
  },

  "uncurry": function(){
    var tuple = { _0: 1, _1: 2, ctor: "Tuple2" },
        f = function(tuple){ return tuple; };
    assert.equals(Functions.A3(Prelude.uncurry, f, 1, 2), tuple);
  },

  "fst": function(){
    var tuple = { _0: 1, _1: 2, ctor: "Tuple2" };
    assert.equals(Prelude.fst(tuple), 1);
  },

  "snd": function(){
    var tuple = { _0: 1, _1: 2, ctor: "Tuple2" };
    assert.equals(Prelude.snd(tuple), 2);
  },

  "readInt": function(){
    var number = Prelude.readInt(elm.Native.JavaScript.toString("1"));
    assert.equals(number._0, 1);
    assert.equals(number.ctor, "Just");
    assert.equals(Prelude.readInt(elm.Native.JavaScript.toString("lame")).ctor, "Nothing");
    assert.equals(Prelude.readInt(elm.Native.JavaScript.toString("0.0")).ctor, "Nothing");
    assert.equals(Prelude.readInt(elm.Native.JavaScript.toString("")).ctor, "Nothing");
    assert.equals(Prelude.readInt(elm.Native.JavaScript.toString("-123"))._0, -123);
  },

  "readFloat": function(){
    var number = Prelude.readFloat(elm.Native.JavaScript.toString("1"));
    assert.equals(number._0, 1.0);
    assert.equals(number.ctor, "Just");
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("lame")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("0e")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("0.0"))._0, 0.0);
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("-0.0"))._0, 0.0);
    assert.equals(Prelude.readFloat(elm.Native.JavaScript.toString("-3.44444444444"))._0, -3.44444444444);
  }
});
