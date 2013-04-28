/*
(install node/npm)
npm install buster -g
node test/runtime.js
*/

var buster = require("buster");

require("../dist/data/elm-runtime.js");

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
  }
});
