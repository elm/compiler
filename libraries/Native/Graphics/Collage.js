
Elm.Native.Graphics.Collage = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 if (elm.Native.Graphics.Collage) return elm.Native.Graphics.Collage;

 var newElement = Elm.Graphics.Element(elm).newElement;
 var C = ElmRuntime.use(ElmRuntime.Render.Collage);

 function collage(w,h,forms) {
     return A3(newElement, w, h, {
                 ctor: 'Custom',
		 type: 'Collage',
		 render: C.render,
		 update: C.update,
		 model: {w:w, h:h, forms:forms}
	 });
 }
 return elm.Native.Graphics.Collage = { collage:F3(collage) };

};