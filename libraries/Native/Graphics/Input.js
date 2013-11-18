Elm.Native.Graphics.Input = {};
Elm.Native.Graphics.Input.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 elm.Native.Graphics.Input = elm.Native.Graphics.Input || {};
 if (elm.Native.Graphics.Input.values) return elm.Native.Graphics.Input.values;

 var Render = ElmRuntime.use(ElmRuntime.Render.Element);
 var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

 var Signal = Elm.Signal.make(elm);
 var newElement = Elm.Graphics.Element.make(elm).newElement;
 var JS = Elm.Native.JavaScript.make(elm);
 var Utils = Elm.Native.Utils.make(elm);
 var Tuple2 = Utils.Tuple2;

 function dropDown(values) {
     var entries = JS.fromList(values);
     var events = Signal.constant(entries[0]._1);

     var drop = newNode('select');
     drop.style.border = '0 solid';
     for (var i = 0; i < entries.length; ++i) {
         var option = newNode('option');
         var name = JS.fromString(entries[i]._0);
         option.value = name;
         option.innerHTML = name;
         drop.appendChild(option);
     }
     drop.addEventListener('change', function() {
             elm.notify(events.id, entries[drop.selectedIndex]._1);
         });

     var t = drop.cloneNode(true);
     t.style.visibility = "hidden";

     elm.node.appendChild(t);
     var style = window.getComputedStyle(t, null);
     var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
     var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
     elm.node.removeChild(t);
     
     var element = A3(newElement, w, h, {
             ctor: 'Custom',
             type: 'DropDown',
             render: function render(model) { return drop; },
             update: function update(node, oldModel, newModel) {},
             model: {}
         });

     return Tuple2(Signal.constant(element), events);
 }

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
     }

     function button(evnt, txt) {
         return A3(newElement, 100, 40, {
                     ctor: 'Custom',
                     type: 'Button',
                     render: render,
                     update: update,
                     model: { event:evnt, text:JS.fromString(txt) }
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

         var clicker = newNode('div');
         clicker.style.width = btn.elmUp.style.width;
         clicker.style.height = btn.elmUp.style.height;
         clicker.style.position = 'absolute';
         clicker.style.top = 0;
         btn.appendChild(clicker);

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

     return { _:{}, customButton:F4(button), events:events };
 }


 function hoverables(defaultValue) {
     var events = Signal.constant(defaultValue);
     function hoverable(handler, elem) {
         function onHover(bool) {
             elm.notify(events.id, handler(bool));
         }
         var props = Utils.replace([['hover',onHover]], elem.props);
         return { props:props, element:elem.element };
     }
     return { _:{}, hoverable:F2(hoverable), events:events };
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

     return { _:{}, checkbox:F2(box), events:events };
 }

 function setRange(node, start, end, dir) {
     if (node.parentNode) {
         node.setSelectionRange(start, end, dir);
     } else {
         setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
     }
 }

 function mkTextPool(type) { return function fields(defaultValue) {
     var events = Signal.constant(defaultValue);

     var state = null;
     var isTextArea = type == 'textarea';
     function render(model) {
         var nodeKind = 'input';
         if(isTextArea){nodeKind='textarea';}
         var field = newNode(nodeKind);
         field.elmHandler = model.handler;

         field.id = 'test';
         if(!isTextArea){field.type = type;}
         field.placeholder = JS.fromString(model.placeHolder);
         field.value = JS.fromString(model.state.string);
         setRange(field, model.state.selectionStart, model.state.selectionEnd, 'forward');
         field.style.border = 'none';
         state = model.state;
         
         function update() {
             var start = field.selectionStart,
                 end = field.selectionEnd;
             if (field.selectionDirection === 'backward') {
                 start = end;
                 end = field.selectionStart;
             }
             state = { _:{},
                       string:JS.toString(field.value),
                       selectionStart:start,
                       selectionEnd:end };
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
         if (state === newModel.state) return;
         var newStr = JS.fromString(newModel.state.string);
         if (node.value !== newStr) node.value = newStr;

         var start = newModel.state.selectionStart;
         var end = newModel.state.selectionEnd;
         var direction = 'forward';
         if (end < start) {
             start = end;
             end = newModel.state.selectionStart;
             direction = 'backward';
         }
 
         if (node.selectionStart !== start
             || node.selectionEnd !== end
             || node.selectionDirection !== direction) {
             setRange(node, start, end, direction);
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

 return elm.Native.Graphics.Input.values = {
     buttons:buttons,
     customButtons:customButtons,
     hoverables:hoverables,
     checkboxes:checkboxes,
     fields:mkTextPool('text'),
     emails:mkTextPool('email'),
     passwords:mkTextPool('password'),
     textareas:mkTextPool('textarea'),
     dropDown:dropDown
 };

};
