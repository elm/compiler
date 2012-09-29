
var Elm = function() {
    var send = function(node, timestep, changed) {
	var kids = node.kids;
	for (var i = kids.length; i--; ) {
	    kids[i].recv(timestep, changed, node.id);
	}
    };
    var input = function(base) {
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
    var lift = function(func,args) {
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
    var fold = function(func,base,baseIsFunc,input) {
	this.id = Guid.guid();
	this.value = baseIsFunc ? base(input.value) : base;
	this.kids = [];
	this.recv = function(timestep, changed, parentID) {
	    if (changed) { this.value = func(input.value)(this.value); }
	    send(this, timestep, changed);
	};
	input.kids.push(this);
    };

    var dropIf = function(pred,base,input) {
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
    var dropRepeats = function(input) {
	this.id = Guid.guid();
	this.value = input.value;
	this.kids = [];
	this.recv = function(timestep, changed, parentID) {
	    var chng = changed && !eq(this.value,input.value);
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

    var sampleOn = function(s1,s2) {
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

    return {Input: function(x) {return new input(x);},
	    Lift:  function(f,xs){return new lift(f,xs);},
	    Fold:  function(f,b,x){return new fold(f,b,false,x);},
	    Fold1: function(f,b,x){return new fold(f,b,true,x);},
	    keepIf : function(pred) { return function(base) { return function(sig) {
		    return new dropIf(function(x) { return !pred(x)},base,sig); }; }; },
	    dropIf : function(pred) { return function(base) { return function(sig) {
		    return new dropIf(pred,base,sig); }; }; },
	    keepWhen : function(s) { return dropWhen(new lift(function(b){return !b;},[s])); },
	    dropWhen : dropWhen,
	    dropRepeats : function(s) { return new dropRepeats(s);},
	    sampleOn : function(s1) { return function(s2) { return new sampleOn(s1,s2); }; }
    };
}();

var Dispatcher = function() {
    var program = null;
    var timestep = 0;
    var inputs = [];
    var currentElement = null;

    var initialize = function() {
	program = ElmCode.main();
	if (!program.hasOwnProperty('recv')) {
	    program = Elm.Input(program);
	}

	currentElement = program.value;
	filterDeadInputs();

	var content = document.getElementById('content');
	content.appendChild(Render.render(currentElement));
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
	program = Elm.Lift(function(value) {
		var content = document.getElementById('content');
		Render.update(content.firstChild,currentElement,value);
		currentElement = value;
		return value;
	    }, [program]);
    };
    var notify = function(id, v) {
	timestep += 1;
	//console.log(timestep);
	var hasListener = false;
	for (var i = inputs.length; i--; ) {
	    hasListener = inputs[i].recv(timestep, id, v) || hasListener;
	}
	return hasListener;
    };

    function isAlive(input) {
	if (!input.hasOwnProperty('defaultNumberOfKids')) return true;
	var len = input.kids.length;
	if (len == 0) return false;
	if (len > input.defaultNumberOfKids) return true;
	var alive = false;
	for (var i = len; i--; ) {
	    alive = alive || isAlive(input.kids[i]);
	}
	return alive;
    }
    function filterDeadInputs() {
	var temp = [];
	for (var i = inputs.length; i--; ) {
	    if (isAlive(inputs[i])) temp.push(inputs[i]);
	}
	inputs = temp;
    }

    return {initialize:initialize, notify:notify, inputs:inputs};
}();