
Elm.Native.Graphics.Matrix = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 if (elm.Native.Graphics.Matrix) return elm.Native.Graphics.Matrix;

 try { Float32Array; } catch(e) { Float32Array = Array; }
 var A = Float32Array;

 var identity = new A([1,0,0,0,1,0]);

 function rotate(t,m) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     var m0 = m[0], m1 = m[1], m3 = m[3], m4 = m[4];
     return new A([m0*c + m1*-s, m0*s + m1*c, m[2],
		   m3*c + m4*-s, m3*s + m4*c, m[5]]);
 }
 function scale(x,y,m) {
     return new A([m[0]*x, m[1]*y, m[2],
		   m[3]*x, m[4]*y, m[5]]);
 }
 function move(x,y,m) {
     var m0 = m[0], m1 = m[1], m3 = m[3], m4 = m[4];
     return new A([m0, m1, m0*x + m1*y + m[2],
		   m3, m4, m3*x + m4*y + m[5]]);
 }
 function matrix(a,b,c,d,e,f,m) {
     var m0 = m[0], m1 = m[1], m3 = m[3], m4 = m[4];
     return new A([m0*a + m1*d, m0*b + m1*e, m0*c + m1*f + m[2],
		   m3*a + m4*d, m3*b + m4*e, m3*c + m4*f + m[5]]);
 }

 return elm.Native.Graphics.Matrix = {
     identity:identity,
     rotation:F2(rotation),
     scale:F3(scale),
     move:F3(move),
     matrix:F7(matrix)
 };

};