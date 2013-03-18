
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
 function matrix(n11, n12, n21, n22, dx, dy, m) {
     var m0 = m[0], m1 = m[1], m3 = m[3], m4 = m[4];
     return new A([m0*n11 + m1*n12, m0*n21 + m1*n22, m0*dx + m1*dy + m[2],
		   m3*n11 + m4*n12, m3*n21 + m4*n22, m3*dx + m4*dy + m[5]]);
 }

 return elm.Native.Graphics.Matrix = {
     identity:identity,
     rotate:F2(rotate),
     scale:F3(scale),
     move:F3(move),
     matrix:F7(matrix)
 };

};