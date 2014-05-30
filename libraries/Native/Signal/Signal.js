
Elm.Native.Signal = {};
Elm.Native.Signal.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Signal = elm.Native.Signal || {};
  if (elm.Native.Signal.values) return elm.Native.Signal.values;

  var Utils = Elm.Native.Utils.make(elm);
  var foldr1 = Elm.List.make(elm).foldr1;

  function send(node, timestep, updated, duplicate) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, updated, duplicate, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var updated = eid === this.id;
      if (updated) { this.value = v; }
      // TODO: consider changing this to an actual duplicate check
      var duplicate = !updated;
      send(this, timestep, updated, duplicate);
      return changed;
    };
    elm.inputs.push(this);
  }

  function LiftN(pure, update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var oneUpdated = false;
    var allDuplicate = true;

    this.recv = function(timestep, updated, duplicate, parentID) {
      ++count;
      if (updated) { oneUpdated = true; }
      if (!duplicate) { allDuplicate = false; }
      if (count === n) {
        if (!(pure && allDuplicate)) { this.value = update(); }
        send(this, timestep, oneUpdated, pure && allDuplicate);
        oneUpdated = false;
        allDuplicate = true;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function lift(func, a) {
    function update() { return func(a.value); }
    return new LiftN(true, update, [a]);
  }
  function lift2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(true, update, [a,b]);
  }
  function lift3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(true, update, [a,b,c]);
  }
  function lift4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(true, update, [a,b,c,d]);
  }
  function lift5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(true, update, [a,b,c,d,e]);
  }
  function lift6(func, a, b, c, d, e, f) {
    function update() { return A6( func, a.value, b.value, c.value, d.value, e.value, f.value ); }
    return new LiftN(true, update, [a,b,c,d,e,f]);
  }
  function lift7(func, a, b, c, d, e, f, g) {
    function update() { return A7( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value ); }
    return new LiftN(true, update, [a,b,c,d,e,f,g]);
  }
  function lift8(func, a, b, c, d, e, f, g, h) {
    function update() { return A8( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value ); }
    return new LiftN(true, update, [a,b,c,d,e,f,g,h]);
  }
  
  function liftImpure(func, a) {
    function update() { return func(a.value); }
    return new LiftN(false, update, [a]);
  }
  function liftImpure2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(false, update, [a,b]);
  }
  
  //For lifting side-effecting functions
  function RawLift(func, recvF, s) {
    this.id = Utils.guid();
    this.value = func(s.value);
    this.kids = [];
    
    this.recv = recvF(this);
    s.kids.push(this);
  }
  
  function rawLift(f, r, a) {
    return new RawLift(f,r,a);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, updated, _, parentID) {
      if (updated) {
          this.value = A2( step, input.value, this.value );
      }
      var duplicate = !updated;
      send(this, timestep, updated, duplicate);
    };
    input.kids.push(this);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    
    var lastPred = pred(input.value);
    
    this.value = lastPred ? base : input.value;
    this.kids = [];
    
    this.recv = function(timestep, updated, duplicate, parentID) {
      if (duplicate) {
        // runtime sanity check
        // can be commented out when reasonably certain this doesn't occur.
        if (!Utils.eq(this.value, input.value) {
          throw new Error(
              'Runtime check of duplicate tracking went wrong!\n' +
              'A changed value was called a duplicate.\n' +
              'Definitely report this to <https://github.com/evancz/Elm/issues>\n');
        }
      }
      if (duplicate && lastPred) { // Duplicate, pred
        updated = false;              // -> NoUpdate 
      }
      if (!duplicate) {            // MaybeChanged
        lastPred = pred(input.value); // -> update pred
        if (lastPred) {            // MaybeChanged,  pred
          updated   = false;          // -> NoUpdate
          duplicate = true;           // -> Duplicate
        } else {                   // MaybeChanged, !pred
          this.value = input.value;   // -> update value
        }
      }
      send(this, timestep, updated, duplicate);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, updated, duplicate, parentID) {
      if (duplicate || Utils.eq(this.value,input.value)) { // Duplicate || MaybeChanged but found to be a duplicate
        duplicate = true;         // -> Duplicate
        updated   = false;        // -> NoUpdate
      } else {                                             // really changed value
        this.value = input.value; // -> update value
      }
      send(this, timestep, updated, duplicate);
    };
    input.kids.push(this);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(Date.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var s1Updated = false;
    var s2Updated = false;
    var s2Duplicate = false;
    var skip = false;

    this.recv = function(timestep, updated, duplicate, parentID) {
      if (parentID === s1.id) { s1Updated = updated; }
      if (parentID === s2.id) {
        s2Updated   = updated;
        s2Duplicate = duplicate;
      }
      ++count;
      if (count === 2) {
        if (s1Updated) {
          if (s2Duplicate && skip) { // if an update from s2 was skipped
            s2Duplicate = false; // then this event could change the output
          }
          this.value = s2.value;
          skip = false;
        } else if (!s2Duplicate) {
          skip = true;
        }
        send(this, timestep, s1Updated, s2Duplicate);
        count = 0;
        s1Updated   = false;
        s2Updated   = false;
        s2Duplicate = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      
      rawLift(function(_){}, function(this) {
        return function(timestep, updated, duplicate, parentID) {
          if(updated) {
            setTimeout(function() { elm.notify(delayed.id, s.value); }, t);
          }
        }
      }, s);
      
      function first(a,b) { return a; }
      // TODO find out why you would even do this whole 
      //  `sampleOn delayed (uncurry fst <~ delayed ~ s)`
      return new SampleOn(delayed, lift2(F2(first), delayed, s));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();

      var count      = 0;
      var current    = { s         : s1
                       , updated   : true
                       , duplicate : false    };
      var next = null;
      
      this.value = current.value;
      this.kids = [];

      this.recv = function(timestep, updated, duplicate, parentID) {
        ++count;
        if (parentID === s2.id && next === null) {
          next = { s         : s2
                 , updated   : updated
                 , duplicate : duplicate };
        } else if (parentID === s1.id) {
          next = { s         : s1
                 , updated   : updated
                 , duplicate : duplicate };
        }

        if (count === 2) {
          if (current.s !== next.s) {
            next.duplicate = false;
          }
          current = next;
          this.value = current.s.value;
          send(this, timestep, current.updated, current.duplicate);
          next = null;
          count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) { return new Merge(s1,s2); }
  function merges(ss) { return A2(foldr1, F2(merge), ss); }

  return elm.Native.Signal.values = {
    constant : function(v) { return new Input(v); },
    lift  : F2(lift ),
    lift2 : F3(lift2),
    lift3 : F4(lift3),
    lift4 : F5(lift4),
    lift5 : F6(lift5),
    lift6 : F7(lift6),
    lift7 : F8(lift7),
    lift8 : F9(lift8),
    liftImpure  : F2(liftImpure ),
    liftImpure2 : F3(liftImpure2),
    rawLift : F3(rawLift),
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    merges : merges,
    count : function(s) { return foldp(F2(function(_,c) { return c+1; }), 0, s); },
    countIf : F2(function(pred,s) {
      return foldp(F2(function(x,c){
        return pred(x) ? c+1 : c; }), 0, s)}),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp
  };
};
