
Elm.Native.Touch = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Touch) return elm.Native.Touch;

  var Signal = Elm.Signal(elm);
  var JS = Elm.JavaScript(elm);

  function Dict() {
    this.keys = [];
    this.values = [];

    this.insert = function(key,value) {
      this.keys.push(key);
      this.values.push(value);
    };
    this.lookup = function(key) {
      var i = this.keys.indexOf(key)
      return i >= 0 ? this.values[i] : {x:0,y:0,t:0};
    };
    this.remove = function(key) {
      var i = this.keys.indexOf(key);
      if (i < 0) return;
      var t = this.values[i];
      this.keys.splice(i,1);
      this.values.splice(i,1);
      return t;
    };
  }
  
  var root = Signal.constant([]),
      tapTime = 500,
      hasTap = false,
      tap = {_:{},x:0,y:0},
      dict = new Dict();

  function touch(t) {
      var r = dict.lookup(t.identifier);
      return {_ : {},
	      id: t.identifier,
	      x : t.pageX,
	      y : t.pageY,
	      x0: r.x,
	      y0: r.y,
	      t0: r.t
	      };
  }

  function start(e) {
    dict.insert(e.identifier,{x:e.pageX,y:e.pageY,t:Date.now()});
  }
  function end(e) {
    var t = dict.remove(e.identifier);
    if (Date.now() - t.t < tapTime) {
	hasTap = true;
	tap = {_:{}, x:t.x, y:t.y};
    }
  }

  function listen(name, f) {
    function update(e) {
      for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
      var ts = new Array(e.touches.length);
      for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
      var hasListener = elm.notify(root.id, ts);
      if (!hasListener) return elm.node.removeEventListener(name, update);
      e.preventDefault();
    }
    elm.node.addEventListener(name, update);
  }

  listen("touchstart", start);
  listen("touchmove", function(_){});
  listen("touchend", end);
  listen("touchcancel", end);
  listen("touchleave", end);

  function dependency(f) {
      var sig = A2( Signal.lift, f, root );
      root.defaultNumberOfKids += 1;
      sig.defaultNumberOfKids = 0;
      return sig;
  }

  var touches = dependency(JS.fromList);

  var taps = function() {
      var sig = dependency(function(_) { return tap; });
      sig.defaultNumberOfKids = 1;
      function pred(_) { var b = hasTap; hasTap = false; return b; }
      var sig2 = A3( Signal.keepIf, pred, {_:{},x:0,y:0}, sig);
      sig2.defaultNumberOfKids = 0;
      return sig2;
  }();

  return elm.Native.Touch = { touches: touches, taps: taps };

};