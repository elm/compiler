Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Mouse = elm.Native.Mouse || {};
    if (elm.Native.Mouse.values) return elm.Native.Mouse.values;

    var Signal = Elm.Signal.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    var position  = Signal.constant(Utils.Tuple2(0,0));
    position.defaultNumberOfKids = 2;

    // do not move x and y into Elm. By setting their default number
    // of kids, it is possible to detatch the mouse listeners if
    // they are not needed.
    var x = A2( Signal.lift, function(p){return p._0}, position);
    x.defaultNumberOfKids = 0;
    var y = A2( Signal.lift, function(p){return p._1}, position);
    y.defaultNumberOfKids = 0;

    var isDown    = Signal.constant(false);
    var clicks = Signal.constant(Utils.Tuple0);

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    elm.addListener([clicks.id], node, 'click', function click() {
        elm.notify(clicks.id, Utils.Tuple0);
    });
    elm.addListener([isDown.id], node, 'mousedown', function down() {
        elm.notify(isDown.id, true);
    });
    elm.addListener([isDown.id], node, 'mouseup', function up() {
        elm.notify(isDown.id, false);
    });
    elm.addListener([position.id], node, 'mousemove', function move(e) {
        elm.notify(position.id, Utils.getXY(e));
    });

    return elm.Native.Mouse.values = {
        position: position,
        x:x,
        y:y,
        isDown: isDown,
        clicks: clicks
    };
};
