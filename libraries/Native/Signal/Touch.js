
Elm.Native.Touch = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Touch) return elm.Native.Touch;

  var Signal = Elm.Signal(elm);
  var JS = Elm.JavaScript(elm);
  var _ = Elm.Native.Utils(elm);

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
    this.clear = function() {
        this.keys = [];
        this.values = [];
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
	      x : t.pageX - elm.node.offsetX,
	      y : t.pageY - elm.node.offsetY,
	      x0: r.x,
	      y0: r.y,
	      t0: r.t
	      };
  }

  var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

  function start(e) {
    dict.insert(e.identifier,
                {x: e.pageX - elm.node.offsetX,
                 y: e.pageY - elm.node.offsetY,
                 t: Date.now()});
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
      if (!hasListener) return node.removeEventListener(name, update);
      e.preventDefault();
    }
    node.addEventListener(name, update);
  }

  listen("touchstart", start);
  listen("touchmove", function(_){});
  listen("touchend", end);
  listen("touchcancel", end);
  listen("touchleave", end);

  var mouseID = -1;
  function move(e) {
      for (var i = root.value.length; i--; ) {
          if (root.value[i].id === mouseID) {
              root.value[i].x = e.pageX - elm.node.offsetX;
              root.value[i].y = e.pageY - elm.node.offsetY;
              elm.notify(root.id, root.value);
              break;
          }
      }
  }
  node.addEventListener("mousedown", function(e) {
          node.addEventListener("mousemove", move);
          e.identifier = mouseID;
          root.value.push(touch(e));
          start(e);
          elm.notify(root.id, root.value);
      });
  node.addEventListener("mouseup", function(e) {
          node.removeEventListener("mousemove", move);
          e.identifier = mouseID;
          end(e);
          for (var i = root.value.length; i--; ) {
              if (root.value[i].id === mouseID) {
                  root.value.splice(i, 1);
                  --mouseID;
                  break;
              }
          }
          elm.notify(root.id, root.value);
      });
  node.addEventListener("blur", function() {
          node.removeEventListener("mousemove", move);
          if (root.values.length > 0) {
              elm.notify(root.id, []);
              --mouseID;
          }
          dict.clear();
      });

  function dependency(f) {
      var sig = A2( Signal.lift, f, root );
      root.defaultNumberOfKids += 1;
      sig.defaultNumberOfKids = 0;
      return sig;
  }

  var touches = dependency(JS.toList);

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