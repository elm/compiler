Elm.Native.Keyboard = {};
Elm.Native.Keyboard.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Keyboard = elm.Native.Keyboard || {};
  if (elm.Native.Keyboard.values) return elm.Native.Keyboard.values;

  // Duplicated from Native.Signal
  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  var Signal = Elm.Signal.make(elm);
  var NList = Elm.Native.List.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  var downEvents = Signal.constant(0);
  var upEvents = Signal.constant(0);
  var blurEvents = Signal.constant(0);

  elm.addListener([downEvents.id], document, 'keydown', function down(e) {
    elm.notify(downEvents.id, e.keyCode);
  });

  elm.addListener([upEvents.id], document, 'keyup', function up(e) {
    elm.notify(upEvents.id, e.keyCode);
  });

  elm.addListener([blurEvents.id], document, 'blur', function blur(e) {
    elm.notify(blurEvents.id, NList.Nil);
  });

  function KeyMerge(down, up, blur) {
    var args = [down,up,blur];
    this.id = Utils.guid();
    // Ignore starting values here
    this.value = NList.Nil
    this.kids = [];
    
    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { 
        // We know this a change must only be one of the following cases
        if (parentID === down.id && !(NList.member(down.value)(this.value))) {
          isChanged = true;
          this.value = NList.Cons(down.value, this.value); 
        } 
        if (parentID === up.id) {
          isChanged = true;
          var notEq = function(kc) { return kc !== up.value };
          this.value = NList.filter(notEq)(this.value);
        } 
        if (parentID === blur.id) {
          isChanged = true;
          this.value = NList.Nil;
        }
      }
      if (count == n) {
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };

    for (var i = n; i--; ) { args[i].kids.push(this); }

  }

  var keysDown = Signal.dropRepeats(new KeyMerge(downEvents,upEvents,blurEvents));

  function keySignal(f) {
    var signal = A2(Signal.lift, f, keysDown);
    // must set the default number of kids to make it possible to filter
    // these signals if they are not actually used.
    keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 1;
    var filtered = Signal.dropRepeats(signal)
    filtered.defaultNumberOfKids = 0;
    return filtered;
  }

  function dir(up, down, left, right) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks.ctor === "::") {
        switch (ks._0) {
          case left : --x; break;
          case right: ++x; break;
          case up   : ++y; break;
          case down : --y; break;
        }
        ks = ks._1;
      }
      return { _:{}, x:x, y:y };
    }
    return keySignal(f);
  }

  function is(key) { return keySignal(NList.member(key)); }

  var lastPressed = downEvents;

  return elm.Native.Keyboard.values = {
    isDown:is,
    directions:F4(dir),
    keysDown:keysDown,
    lastPressed:lastPressed
  };

};
