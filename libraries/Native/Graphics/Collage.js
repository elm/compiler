Elm.Native.Graphics.Collage = {};
Elm.Native.Graphics.Collage.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 elm.Native.Graphics.Collage = elm.Native.Graphics.Collage || {};
 if (elm.Native.Graphics.Collage.values) return elm.Native.Graphics.Collage.values;

 var newElement = Elm.Graphics.Element.make(elm).newElement;
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
 return elm.Native.Graphics.Collage.values = { collage:F3(collage) };

};