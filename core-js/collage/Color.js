
var Color = function() {

 var create = function(r,g,b,a) {
    return { r: r,
	     g: g,
	     b: b,
	     a: a };
 };

 var extract = function(c) {
    if (c.a === 1) {
	return 'rgb(' + c.r + ',' + c.g + ',' + c.b + ')';
    }
    return 'rgba(' + c.r + ',' + c.g + ',' + c.b + ',' + c.a + ')';
 };

 var rgba = function(r) { return function(g) { return function(b) {
	    return function(a) { return create(r,g,b,a); }; }; };
 };

 var rgb = function(r) { return function(g) { return function(b) {
	    return create(r,g,b,1); }; };
 };
 return {black: create(  0,  0,  0,1),
	 white: create(255,255,255,1),
	 red  : create(255,  0,  0,1),
	 green: create(  0,255,  0,1),
	 blue : create(  0,  0,255,1),

	 gray : create(128,128,128,1),
	 grey : create(128,128,128,1),

	 yellow  : create(255,255,  0,1),
	 cyan    : create(  0,255,255,1),
	 magenta : create(255,  0,255,1),

	 rgba : rgba,
	 rgb  : rgb,
	 Internal : { extract:extract }
	 };
}();