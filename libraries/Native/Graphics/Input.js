
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

	 function replace(node) {
           if (node !== btn.firstChild) btn.replaceChild(node, btn.firstChild);
	 }
	 var overCount = 0;
	 function over(e) {
	     if (overCount++ > 0) return;
	     replace(btn.elmHover);
	 }
	 function out(e) {
	     if (btn.contains(e.toElement || e.relatedTarget)) return;
	     overCount = 0;
	     replace(btn.elmUp);
	 }
	 function up() {
	     replace(btn.elmHover);
	     elm.notify(events.id, btn.elmEvent);
	 }
	 function down() { replace(btn.elmDown); }
	 btn.addEventListener('mouseover', over);
	 btn.addEventListener('mouseout' , out);
	 btn.addEventListener('mousedown', down);
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


 function checkboxes(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
	 var b = newNode('input');
	 b.type = 'checkbox';
	 b.checked = model.checked;
	 b.style.display = 'block';
	 b.elmHandler = model.handler;
	 function change() { elm.notify(events.id, b.elmHandler(b.checked)); }
	 b.addEventListener('change', change);
	 return b;
     }

     function update(node, oldModel, newModel) {
	 node.elmHandler = newModel.handler;
	 node.checked = newModel.checked;
	 return true;
     }

     function box(handler, checked) {
	 return A3(newElement, 13, 13, {
                     ctor: 'Custom',
		     type: 'CheckBox',
		     render: render,
		     update: update,
		     model: { checked:checked, handler:handler  }
	     });
     }

     return { _:{}, box:F2(box), events:events };
 }

 function mkTextPool(type) { return function fields(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
	 var field = newNode('input');
	 field.elmHandler = model.handler;

	 field.id = 'test';
	 field.type = type;
	 field.placeholder = fromString(model.placeHolder);
	 field.value = fromString(model.state.string);
	 field.setSelectionRange(model.state.start, model.state.end);
	 field.style.border = 'none';

	 function update() {
	     var start = field.selectionStart,
		 end = field.selectionEnd;
	     if (field.selectionDirection === 'backward') {
		 start = end;
		 end = field.selectionStart;
	     }
	     var state = { _:{},
			   string:toString(field.value),
			   start:start,
			   end:end };
	     elm.notify(events.id, field.elmHandler(state));
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
	 node.elmHandler = newModel.handler;
	 node.value = fromString(newModel.state.string);
	 if (newModel.state.start <= newModel.state.end) {
	     node.setSelectionRange(newModel.state.start, newModel.state.end);
	 } else {
	     node.setSelectionRange(newModel.state.end,
				    newModel.state.start, 'backward');
	 }
     }

     function field(handler, placeHolder, state) {
	 return A3(newElement, 200, 30,
                   { ctor: 'Custom',
		     type: type + 'Input',
		     render: render,
		     update: update,
		     model: { handler:handler,
			      placeHolder:placeHolder,
			      state:state }
		   });
     }

     return { _:{}, field:F3(field), events:events };
   }
 }

 return elm.Native.Graphics.Input = {
     buttons:buttons,
     customButtons:customButtons,
     checkboxes:checkboxes,
     fields:mkTextPool('text'),
     emails:mkTextPool('email'),
     passwords:mkTextPool('password')
 };

};
