
var id  = function(x) { return x; };
var not = function(exp) { return !exp; };

var sqrt = function(x) { return Math.sqrt(x); };
var mod = function(x) { return function(y) { return x % y; }; };
var abs = function(x) { return Math.abs(x); };
var logBase = function(base) { return function(x) { return Math.log(x) / Math.log(base); }; };

var min = function(x) { return function(y) { return Math.min(x,y); }; };
var max = function(x) { return function(y) { return Math.max(x,y); }; };
var clamp = function(lo) { return function(hi) {
	return function(x) { return Math.min(hi, Math.max(lo, x)); }; 
    };
};

var sin = Math.sin, cos = Math.cos, tan = Math.tan;
var asin = Math.asin, acos = Math.acos, atan = Math.atan;

var flip = function(f){return function(x){return function(y){return f(y)(x);};};};

var Just = function(x) { return ["Just",x]; };
var Nothing = ["Nothing"];

function constant(v) { return Elm.Input(v); }
function lift(f) { return function(e) { return Elm.Lift(f, [e]); }; }
function lift2(f) { return function(e1) { return function(e2) {
	    return Elm.Lift(f, [e1,e2]); }; }; }
function lift3(f) { return function(e1) { return function(e2) {
	    return function(e3) { return Elm.Lift(f, [e1,e2,e3]); }; }; }; }
function lift4(f) { return function(e1) { return function(e2) {
	    return function(e3) { return function(e4) {
		    return Elm.Lift(f, [e1,e2,e3,e4]); }; }; }; }; }
function foldp(f) { return function(b) { return function(e) {
	    return Elm.Fold(f,b,e); }; }; }


var includeGlobal = this;
(function() {
  var include = function(library) {
    for (var i in library) {
	if (i === 'Internal') continue;
	try {
	    includeGlobal[i] = library[i];
	} catch (err) {
	    if (i === 'length') {
		includeGlobal.execScript('var length;');
		length = library[i];
		continue;
	    }
	}
    }
  };
  var includeAs = function(name) { return function(library) {
	includeGlobal[name] = includeGlobal[name] || {};
	for (var i in library) {
	    if (i === 'Internal') continue;
	    includeGlobal[name][i] = library[i];
	}
    };
  };
  include (Element);
  include (Text);

  color = Element.color;
  height = Element.height;
  show = Value.show;

  includeAs ('Time')   (Signal.Time);
  includeAs ('Mouse')  (Signal.Mouse);
  includeAs ('Window') (Signal.Window);
  includeAs ('HTTP')   (Signal.HTTP);
  includeAs ('Input')  (Signal.Input);
  includeAs ('Random') (Signal.Random);
  
  include (Color);
  include (Shape);
  include (Line);
}());