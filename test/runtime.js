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

buster.testCase("Native.Prelude", {
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
    var number = Prelude.readInt(_str("1"));
    assert.equals(number._0, 1);
    assert.equals(number.ctor, "Just");
    assert.equals(Prelude.readInt(_str("lame")).ctor, "Nothing");
    assert.equals(Prelude.readInt(_str("0.0")).ctor, "Nothing");
    assert.equals(Prelude.readInt(_str("")).ctor, "Nothing");
    assert.equals(Prelude.readInt(_str("-123"))._0, -123);
  },

  "readFloat": function(){
    var number = Prelude.readFloat(_str("1"));
    assert.equals(number._0, 1.0);
    assert.equals(number.ctor, "Just");
    assert.equals(Prelude.readFloat(_str("lame")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(_str("0e")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(_str("")).ctor, "Nothing");
    assert.equals(Prelude.readFloat(_str("0.0"))._0, 0.0);
    assert.equals(Prelude.readFloat(_str("-0.0"))._0, 0.0);
    assert.equals(Prelude.readFloat(_str("-3.44444444444"))._0, -3.44444444444);
  }
});

var Show = Elm.Native.Show(elm);

buster.testCase("Native.Show", {
  "show": function(){
    var toList = N.JavaScript(elm).toList,
        fromList = N.JavaScript(elm).fromList,
        showJS = function(v){ return fromList(Show.show(v)); };
    assert.equals(showJS(""), ["''"]);
    assert.equals(showJS(showJS), ["<function>"]);
    assert.equals(showJS(true), ["True"]);
    assert.equals(showJS(false), ["False"]);
    assert.equals(showJS(1337), ["1337"]);
    assert.equals(showJS("hello"), ["hello"]);
    // I have no idea what these are for.
    assert.equals(showJS({_:{}}), ["{  }"]);
    assert.equals(showJS({_:{a: [1]}, a: 2}), ["{ a = 1, a = 2 }"]);

    assert.equals(showJS({ctor:"Tuple2", _0: 1, _1: 2}), ["(1,2)"]);
    assert.equals(showJS({ctor:"Tuple3", _0: 1, _1: 2, _2: 3}), ["(1,2,3)"]);

    assert.equals(showJS(N.JavaScript(elm).toList(['hello'])), ["\"hello\""]);
    assert.equals(showJS(N.JavaScript(elm).toList([1, 'hello', true])), ["[1,hello,True]"]);

    assert.equals(showJS({ctor: "Nil"}), ["[]"]);

    var list = N.JavaScript(elm).toList([
      N.Utils(elm).Tuple2(_str('you'), _str('goodbye')),
      N.Utils(elm).Tuple2(_str('I'), _str('hello'))
    ]);
    var dict = Elm.Dict(elm).fromList(list);
    assert.equals(showJS(dict), ['Dict.fromList [("I","hello"),("you","goodbye")]']);

    var set = Elm.Set(elm).fromList(list);
    assert.equals(showJS(set), ['Set.fromList [("I","hello"),("you","goodbye")]']);

    assert.equals(showJS({hello: set, ctor:'SomeCtor'}), ['SomeCtor (Set.fromList [("I","hello"),("you","goodbye")])']);
    assert.equals(showJS({a: 'b'}), ['[object Object]']);
  }
});

var Signal = Elm.Signal(elm);

buster.testCase("Signal", {
  constant: function(){
    assert.equals(Signal.constant(1).value, 1);
  }
});

