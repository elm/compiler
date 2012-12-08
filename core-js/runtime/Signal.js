
Elm.Signal = function() {
  var send = function(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  };
  /**
   * @constructor
   */
  function input(base) {
    this.id = Guid.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    Dispatcher.inputs.push(this);
  };
  /**
   * @constructor
   */
  function lift(func,args) {
    this.id = Guid.guid();
    this.value = null;
    this.kids = [];
    this.count = 0;
    this.changed = false;
    
    args.reverse();
    this.recalc = function() {
	var f = func;
	for (var i = args.length; i--; ) {
	    f = f(args[i].value);
	}
	this.value = f;
    };
    this.recalc();
    
    this.recv = function(timestep, changed, parentID) {
      this.count += 1;
      if (changed) { this.changed = true; }
      if (this.count == args.length) {
	  if (this.changed) { this.recalc() }
	  send(this, timestep, this.changed);
	  this.changed = false;
	  this.count = 0;
      }
    };
    for (var i = args.length; i--; ) {
      args[i].kids.push(this);
    }
  };
  /**
   * @constructor
   */
  function fold(func,base,baseIsFunc,input) {
    this.id = Guid.guid();
    this.value = baseIsFunc ? base(input.value) : base;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      if (changed) { this.value = func(input.value)(this.value); }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  };

  /**
   * @constructor
   */
  function dropIf(pred,base,input) {
    this.id = Guid.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
	var chng = changed && !pred(input.value);
	if (chng) { this.value = input.value; }
	send(this, timestep, chng);
    };
    input.kids.push(this);
  };

  /**
   * @constructor
   */
  function dropRepeats(input) {
      this.id = Guid.guid();
      this.value = input.value;
      this.kids = [];
      this.recv = function(timestep, changed, parentID) {
	  var chng = changed && !Value.eq(this.value,input.value);
	  if (chng) { this.value = input.value; }
	  send(this, timestep, chng);
      };
      input.kids.push(this);
  };
  
  var dropWhen = function(s1) { return function(b) { return function(s2) {
    var pairs = new lift(function(x){return function(y){return [x,y];};},[s1,s2]);
    var dropped = new dropIf(function(p){return p[0];},[true,b],pairs);
    return new lift(function(p){return p[1];},[dropped]); }; };
  };

  /**
   * @constructor
   */
  function timeIndex(pair,s) {
      this.id = Guid.guid();
      var t = (new window.Date).getTime();
      this.value = pair ? Value.Tuple(t,s.value) : t;
      this.kids = [];
      this.recv = function(timestep, changed, parentID) {
	  if (changed) this.value = pair ? Value.Tuple(timestep, s.value) : timestep;
	  send(this, timestep, changed);
      };
      s.kids.push(this);
  }

  /**
   * @constructor
   */
  function sampleOn(s1,s2) {
    this.id = Guid.guid();
    this.value = s2.value;
    this.kids = [];
    this.count = 0;
    this.changed = false;
    
    this.recv = function(timestep, changed, parentID) {
	if (parentID === s1.id) this.changed = changed;
	this.count += 1;
	if (this.count == 2) {
	    if (this.changed) { this.value = s2.value; }
	    send(this, timestep, this.changed);
	    this.count = 0;
	    this.changed = false;
	}
    };
    s1.kids.push(this);
    s2.kids.push(this);
  };

  function delay(t) { return function(s) {
      var delayed = new input(s.value);
      var firstEvent = true;
      function update(v) {
	  if (firstEvent) return;
	  setTimeout(function() { Dispatcher.notify(delayed.id,v); },t);
      }
      function first(a) { return function(b) { return a; }; }
      var out = new sampleOn(delayed,new lift(first, [delayed, new lift(update,[s])]));
      firstEvent = false;
      return out;
    };
  }
  
  /**
   * @constructor
   */
  function merge(s1,s2) {
    this.id = Guid.guid();
    this.value = s1.value;
    this.kids = [];
    this.next = null;
    this.count = 0;
    this.changed = false;
    
    this.recv = function(timestep, changed, parentID) {
      this.count += 1;
      if (changed) {
	  this.changed = true;
	  if (parentID == s2.id && this.next === null) { this.next = s2.value; }
	  if (parentID == s1.id) { this.next = s1.value; }
      }

      if (this.count == 2) {
	  if (this.changed) {
	      this.value = this.next;
	      this.next = null;
	  }
	  send(this, timestep, this.changed);
	  this.changed = false;
	  this.count = 0;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  };

  function merges(ss) {
      function mrg(x) { return function(y) { return new merge(x,y); }; }
      return Elm.List.foldl1(mrg)(ss);
  }

  function average(sampleSize) { return function(s) {
    var sample = new Array(sampleSize);
    var i = sampleSize;
    while (i--) sample[i] = 0;
    i = 0;
    var full = false;
    var total = 0;
    function f(n) {
	total += n - sample[i];
	sample[i] = n;
	var avg = total / Math.max(1, full ? sampleSize : i);
	if (++i == sampleSize) { full = true; i = 0; }
	return avg;
    }
    return new lift(f, [s]);
   };
  }

  return {
    constant : function(v) { return new input(v); },
    lift  : function(f){return function(e){return new lift(f,[e]);};},
    lift2 : function(f) {
	      return function(e1) { return function(e2) {
	      return new lift(f, [e1,e2]); };};},
    lift3 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3){return new lift(f,[e1,e2,e3]);};};};},
    lift4 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3) { return function(e4) {
	      return new lift(f, [e1,e2,e3,e4]);};};};};},
    lift5 : function(f) {
	      return function(e1) { return function(e2) {
	      return function(e3) { return function(e4) {
	      return function(e5) {
	      return new lift(f, [e1,e2,e3,e4,e5]);};};};};};},
    lift6 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
	      return new lift(f, [e1,e2,e3,e4,e5,e6]); };};};};};};},
    lift7 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
              return function(e7) {
              return new lift(f, [e1,e2,e3,e4,e5,e6,e7]);};};};};};};};},
    lift8 : function(f) { return function(e1) { return function(e2) {
              return function(e3) { return function(e4) {
              return function(e5) { return function(e6) {
              return function(e7) { return function(e8) {
              return new lift(f, [e1,e2,e3,e4,e5,e6,e7,e8]);};};};};};};};};},
    foldp : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,false,e); }; }; },
    foldp_ : function(f) { return function(b) { return function(e) {
		  return new fold(f,b,true,e); }; }; },
    foldp1 : function(f) { return function(e) {
	      return new fold(f,function(x){return x;},true,e); }; },
    delay : delay,
    merge : function(s1) { return function(s2) { return new merge(s1,s2)}; },
    merges : merges,
    average : average,
    count : function(sig) {
	  var incr = function(_){return function(c){return c+1;};};
	  return new fold(incr,0,false,sig) },
    countIf : function(pred) { return function(sig) {
	      var incr = function(x){return function(c){return pred(x) ? c+1 : c;};};
	      return new fold(incr,0,false,sig) }; },
    keepIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(function(x) { return !pred(x)},base,sig); }; }; },
    dropIf : function(pred) { return function(base) { return function(sig) {
		  return new dropIf(pred,base,sig); }; }; },
    keepWhen : function(s) { return dropWhen(new lift(function(b){return !b;},[s])); },
    dropWhen : dropWhen,
    dropRepeats : function(s) { return new dropRepeats(s);},
    sampleOn : function(s1){return function(s2){return new sampleOn(s1,s2);};},
    timestamp : function(s) { return new timeIndex(true, s); },
    timeOf : function(s) { return new timeIndex(false, s); }
  };
}();