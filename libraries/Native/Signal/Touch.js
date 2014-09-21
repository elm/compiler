Elm.Native.Touch = {};
Elm.Native.Touch.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Touch = elm.Native.Touch || {};
    if (elm.Native.Touch.values) return elm.Native.Touch.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

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
        var point = Utils.getXY(t);
        return {_ : {},
	        id: t.identifier,
	        x : point._0,
	        y : point._1,
	        x0: r.x,
	        y0: r.y,
	        t0: r.t
	       };
    }

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    function start(e) {
        var point = Utils.getXY(e);
        dict.insert(e.identifier,
                    {x: point._0,
                     y: point._1,
                     t: elm.timer.now()});
    }
    function end(e) {
        var t = dict.remove(e.identifier);
        if (elm.timer.now() - t.t < tapTime) {
            hasTap = true;
            tap = {_:{}, x:t.x, y:t.y};
        }
    }

    function listen(name, f) {
        function update(e) {
            for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
            var ts = new Array(e.touches.length);
            for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
            elm.notify(root.id, ts);
            e.preventDefault();
        }
        elm.addListener([root.id], node, name, update);
    }

    listen("touchstart", start);
    listen("touchmove", function(_){});
    listen("touchend", end);
    listen("touchcancel", end);
    listen("touchleave", end);

    var mouseID = -1;
    function move(e) {
        var point = Utils.getXY(e);
        for (var i = root.value.length; i--; ) {
            if (root.value[i].id === mouseID) {
                root.value[i].x = point._0;
                root.value[i].y = point._1;
                elm.notify(root.id, root.value);
                break;
            }
        }
    }
    elm.addListener([root.id], node, "mousedown", function down(e) {
        node.addEventListener("mousemove", move);
        e.identifier = mouseID;
        start(e);
        root.value.push(touch(e));
        elm.notify(root.id, root.value);
    });
    elm.addListener([root.id], document, "mouseup", function up(e) {
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
    elm.addListener([root.id], node, "blur", function blur(e) {
        node.removeEventListener("mousemove", move);
        if (root.value.length > 0) {
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

    var touches = dependency(List.fromArray);

    var taps = function() {
        var sig = dependency(function(_) { return tap; });
        sig.defaultNumberOfKids = 1;
        function pred(_) { var b = hasTap; hasTap = false; return b; }
        var sig2 = A3( Signal.keepIf, pred, {_:{},x:0,y:0}, sig);
        sig2.defaultNumberOfKids = 0;
        return sig2;
    }();

    return elm.Native.Touch.values = { touches: touches, taps: taps };

};
