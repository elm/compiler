
Elm.Native.Graphics.Input = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 if (elm.Native.Graphics.Input) return elm.Native.Graphics.Input;

 var Render = ElmRuntime.use(ElmRuntime.Render.Element);
 var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
 var newNode = Utils.newElement, fromString = Utils.fromString,
     toString = Utils.toString;

 var Signal = Elm.Signal(elm);
 var newElement = Elm.Graphics.Element(elm).newElement;

 function buttons(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
	 var b = newNode('button');
	 b.style.display = 'block';
	 b.elmEvent = model.event;
	 function click() { elm.notify(events.id, b.elmEvent); }
	 b.addEventListener('click', click);
	 b.innerHTML = model.text;
	 return b;
     }

     function update(node, oldModel, newModel) {
	 node.elmEvent = newModel.event;
	 var txt = newModel.text;
	 if (oldModel.text !== txt) node.innerHTML = txt;
	 return true;
     }

     function button(evnt, txt) {
	 return A3(newElement, 100, 40, {
                     ctor: 'Custom',
		     type: 'Button',
		     render: render,
		     update: update,
		     model: { event:evnt, text:fromString(txt) }
	     });
     }

     return { _:{}, button:F2(button), events:events };
 }

 function customButtons(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
	 var btn = newNode('div');
	 btn.elmEvent = model.event;

	 btn.elmUp    = Render.render(model.up);
	 btn.elmHover = Render.render(model.hover);
	 btn.elmDown  = Render.render(model.down);
	 
	 function replace(node) { return function() {
           if (node !== btn.firstChild) btn.replaceChild(node, btn.firstChild);
	  };
	 }
	 var hover = replace(btn.elmHover);
	 function up() { hover(); elm.notify(events.id, btn.elmEvent); }
	 btn.addEventListener('mouseover', hover);
	 btn.addEventListener('mouseout' , replace(btn.elmUp));
	 btn.addEventListener('mousedown', replace(btn.elmDown));
	 btn.addEventListener('mouseup'  , up);

	 btn.appendChild(btn.elmUp);
	 return btn;
     }

     function update(node, oldModel, newModel) {
	 node.elmEvent = newModel.event;
	 Render.update(node.elmUp, oldModel.up, newModel.up)
	 Render.update(node.elmHover, oldModel.hover, newModel.hover)
	 Render.update(node.elmDown, oldModel.down, newModel.down)
     }

     function button(evnt, up, hover, down) {
	 return A3(newElement,
		   Math.max(up.props.width, hover.props.width, down.props.width),
		   Math.max(up.props.height, hover.props.height, down.props.height),
                   { ctor: 'Custom',
		     type: 'CustomButton',
		     render: render,
		     update: update,
		     model: { event:evnt, up:up, hover:hover, down:down }
		   });
     }

     return { _:{}, button:F4(button), events:events };
 }

 function textFields(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
	 var field = newNode('input');
	 field.elmHandler = model.handler;

	 field.id = 'test'
	 field.placeholder = fromString(model.placeHolder);
	 field.elmValue = fromString(model.state.input);
	 field.value = field.elmValue;
	 field.setSelectionRange(model.state.start, model.state.end);
	 field.style.border = 'none';

	 function update() {
	     var state = { _:{},
			   input:toString(field.value),
			   start:field.selectionStart,
			   end:field.selectionEnd };
	     field.value = field.elmValue;
	     elm.notify(events.id, state);
	 }
	 function mousedown() {
	     update();
	     elm.node.addEventListener('mouseup', mouseup);
	 }
	 function mouseup() {
	     update();
	     elm.node.removeEventListener('mouseup', mouseup)
	 }
	 field.addEventListener('keyup', update);
	 field.addEventListener('mousedown', mousedown);

	 return field;
     }

     function update(node, oldModel, newModel) {
	 node.elmEvent = newModel.handler;
	 node.elmValue = fromString(newModel.state.input);
	 node.value = node.elmValue;
	 node.setSelectionRange(newModel.state.start, newModel.state.end);
     }

     function field(handler, placeHolder, state) {
	 return A3(newElement, 200, 30,
                   { ctor: 'Custom',
		     type: 'TextField',
		     render: render,
		     update: update,
		     model: { handler:handler,
			      placeHolder:placeHolder,
			      state:state }
		   });
     }

     return { _:{}, field:F3(field), events:events };
 }

 return elm.Native.Graphics.Input = {
     buttons:buttons,
     customButtons:customButtons,
     textFields:textFields
 };

};