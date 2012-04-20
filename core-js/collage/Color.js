
var Color = function() {

 var create = function(r,g,b,a) {
    return { r: Math.round(255*r),
	     g: Math.round(255*g),
	     b: Math.round(255*b),
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
 return {black: create(0,0,0,1),
	 white: create(1,1,1,1),
	 red  : create(1,0,0,1),
	 green: create(0,1,0,1),
	 blue : create(0,0,1,1),
	 rgba : rgba,
	 rgb  : rgb,
	 Internal : { extract:extract }
	 };
}();