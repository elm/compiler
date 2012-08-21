
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
	this.recv = function(timestep, eid, v) {
	    var changed = eid === this.id;
	    if (changed) { this.value = v; }
	    send(this, timestep, changed);
	};
	Dispatcher.inputs.push(this);
    };
    var lift = function(func,args) {
	this.id = Guid.guid();
	this.value = null;
	this.kids = [];
	this.inbox = {};

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
	    if (!this.inbox.hasOwnProperty(timestep)) {
		this.inbox[timestep] = { changed: false, count: 0 };
	    }
	    var box = this.inbox[timestep];
	    box.count += 1;
	    if (changed) { box.changed = true; }
	    if (box.count == args.length) {
		if (box.changed) { this.recalc() }
		send(this, timestep, box.changed);
		delete this.inbox[timestep];
	    }
	};
	for (var i = args.length; i--; ) {
	    args[i].kids.push(this);
	}
    };
    var fold = function(func,base,input) {
	this.id = Guid.guid();
	this.value = base;
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
	this.inbox = {};

	this.recv = function(timestep, changed, parentID) {
	    var chng = changed && parentID === s1.id;
	    if (chng) { this.value = s2.value; }
	    send(this, timestep, chng);
	};
	s1.kids.push(this);
	s2.kids.push(this);
    };

    return {Input: function(x) {return new input(x);},
	    Lift:  function(f,xs){return new lift(f,xs);},
	    Fold:  function(f,b,x){return new fold(f,b,x);},
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

    var correctSize = function(e) {
	var kids = e.childNodes;
	var len = kids.length;
	if (e.hasOwnProperty('isElmLeaf')) {
	    if (e.hasOwnProperty('isElmText')) { Element.correctTextSize(e); }
	    var w = e.style.width === "" ?
		0 : e.style.width.slice(0,-2) - 0;
	    var h = e.style.height === "" ?
		0 : e.style.height.slice(0,-2) - 0;
	    return [w, h];
	}
	if (len === 1) {
	    var dim = correctSize(kids[0]);
	    if (e.style.width  !== "") { dim[0] = e.style.width.slice(0,-2) - 0; }
	    if (e.style.height !== "") { dim[1] = e.style.height.slice(0,-2) - 0; }
	    if (dim[0] !== 0) { e.style.width = dim[0] + "px"; }
	    if (dim[1] !== 0) { e.style.height = dim[1] + "px"; }
	    return dim;
	}
	var wmax = 0, hmax = 0, wsum = 0, hsum = 0;
	var hasWidth = true, hasHeight = true, dim = null;
	while (len--) {
	    dim = correctSize(kids[len]);
	    wmax = Math.max(wmax, dim[0]);
	    hmax = Math.max(hmax, dim[1]);
	    wsum += dim[0];
	    hsum += dim[1];
	    hasWidth  = hasWidth  && dim[0] > 0;
	    hasHeight = hasHeight && dim[1] > 0;
	}
	var w = wmax, h = hmax, dir = e.elmFlowDirection;
	if (dir === "X") { w = hasWidth ? wsum : 0; }
	if (dir === "Y") { h = hasHeight ? hsum : 0; }
	if (w > 0) e.style.width  = w + "px";
	if (h > 0) e.style.height = h + "px";
	return [w,h];
    };

    var initialize = function() {
	program = ElmCode.main();
	if (!program.hasOwnProperty('recv')) {
	    program = Elm.Input(program);
	}
	var content = document.getElementById('content');
	content.appendChild(program.value);
	adjust();
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
	program = Elm.Lift(function(value) {
		var content = document.getElementById('content');
		var kid = content.children[0]
		content.replaceChild(value, kid);
		delete kid;
		adjust();
		return value;
	    }, [program]);
    };
    var adjust = function() {
	var content = document.getElementById('content');
	correctSize(content.children[0]);
    }
    var notify = function(id, v) {
	timestep += 1;
	for (var i = inputs.length; i--; ) {
	    inputs[i].recv(timestep, id, v);
	}
    };
    return {initialize:initialize, notify:notify, adjust:adjust, inputs:inputs};
}();