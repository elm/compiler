
var Elm = function() {
    var input = function(base) {
	this.id = Guid.guid();
	this.value = base;
	this.step = function(eid,v) {
	    var changed = eid === this.id;
	    if (changed) { this.value = v; }
	    return changed;
	};
    };
    var lift = function(func,args) {
	this.id = Guid.guid();
	this.value = null;

	args.reverse();
	this.recalc = function() {
	    var f = func;
	    for (var i = args.length; i--; ) {
		f = f(args[i].value);
	    }
	    this.value = f;
	};
	this.recalc();

	this.step = function(id,v) {
	    if (this.hasOwnProperty(id)) return false;
	    var changed = false;
	    for (var i = args.length; i--; ) {
		// Must not short-circuit!
		// All arguments need to be steped forward!
		changed = args[i].step(id,v) || changed;
	    }
	    changed ? (this.recalc()) : (this[id] = true);
	    return changed;
	};
    };
    var fold = function(func,base,input) {
	this.id = Guid.guid();
	this.value = base;
	this.step = function(id,v) {
	    if (this.hasOwnProperty(id)) return false;
	    var changed = input.step(id,v);
	    if (changed) { this.value = func(input.value)(this.value); }
	    else { this[id] = true; }
	    return changed;
	};
    };
    return {Input: function(x) {return new input(x);},
	    Lift:  function(f,xs){return new lift(f,xs);},
	    Fold:  function(f,b,x){return new fold(f,b,x);}
    };
}();

var Dispatcher = function() {
    var program = null;
    var correctSize = function(e) {
	var kids = e.childNodes;
	var len = kids.length;
	if (e.hasOwnProperty('isElmLeaf')) {
	    var w = e.style.width === "" ?
		0 : e.style.width.slice(0,-2) - 0;
	    var h = e.style.height === "" ?
		0 : e.style.height.slice(0,-2) - 0;
	    return [w, h];
	}
	if (len === 1) {
	    var dim = correctSize(kids[0]);
	    if (e.style.width !== "") { dim[0] = e.style.width.slice(0,-2) - 0; }
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
	try { program = main(); } catch (e) {
	    document.body.innerHTML = "An Error Occured: Better Messages to come.";
	    throw e;
	}
	if (!program.hasOwnProperty('step')) {
	    program = Elm.Input(program);
	}
	var content = document.getElementById('content');
	content.appendChild(program.value);
	correctSize(content);
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
    };
    var adjust = function() {
	var content = document.getElementById('content');
	correctSize(content);
    }
    var notify = function(id, v) {
	if (program.step(id,v)) {
	    var content = document.getElementById('content');
	    content.replaceChild(program.value, content.children[0]);
	    correctSize(content);
	}
    };
    return {initialize:initialize, notify:notify, adjust:adjust};
}();