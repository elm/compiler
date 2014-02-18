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

    function input(initialValue) {
        var signal = Signal.constant(initialValue);
        return { _:{}, signal:signal, handle:signal };
    }

    function renderDropDown(signal, values) {
        return function(_) {
            var entries = JS.fromList(values);

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
                elm.notify(signal.id, entries[drop.selectedIndex]._1);
            });

            var t = drop.cloneNode(true);
            t.style.visibility = "hidden";

            elm.node.appendChild(t);
            var style = window.getComputedStyle(t, null);
            var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
            var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
            elm.node.removeChild(t);
            return drop;
        };
    }

    function updateDropDown(node, oldModel, newModel) {
    }

    function dropDown(signal, values) {
        return A3(newElement, 100, 24, {
            ctor: 'Custom',
            type: 'DropDown',
            render: renderDropDown(signal,values),
            update: updateDropDown,
            model: {}
        });
    }

    function renderButton(model) {
        var node = newNode('button');
        node.style.display = 'block';
        node.elm_signal = model.signal;
        node.elm_value = model.value;
        function click() {
            elm.notify(node.elm_signal.id, node.elm_value);
        }
        node.addEventListener('click', click);
        node.innerHTML = model.text;
        return node;
    }

    function updateButton(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_value = nemModel.value;
        var txt = newModel.text;
        if (oldModel.text !== txt) node.innerHTML = txt;
    }

    function button(signal, value, text) {
        return A3(newElement, 100, 40, {
            ctor: 'Custom',
            type: 'Button',
            render: renderButton,
            update: updateButton,
            model: { signal:signal, value:value, text:JS.fromString(text) }
        });
    }

    function renderCustomButton(model) {
        var btn = newNode('div');
        btn.elm_signal = model.signal;
        btn.elm_value = model.value;

        btn.elm_up    = Render.render(model.up);
        btn.elm_hover = Render.render(model.hover);
        btn.elm_down  = Render.render(model.down);

        function replace(node) {
            if (node !== btn.firstChild) {
                btn.replaceChild(node, btn.firstChild);
            }
        }
        var overCount = 0;
        function over(e) {
            if (overCount++ > 0) return;
            replace(btn.elm_hover);
        }
        function out(e) {
            if (btn.contains(e.toElement || e.relatedTarget)) return;
            overCount = 0;
            replace(btn.elm_up);
        }
        function up() {
            replace(btn.elm_hover);
            elm.notify(btn.elm_signal.id, btn.elm_value);
        }
        function down() {
            replace(btn.elm_down);
        }
        btn.addEventListener('mouseover', over);
        btn.addEventListener('mouseout' , out);
        btn.addEventListener('mousedown', down);
        btn.addEventListener('mouseup'  , up);

        btn.appendChild(btn.elm_up);

        var clicker = newNode('div');
        clicker.style.width = btn.elm_up.style.width;
        clicker.style.height = btn.elm_up.style.height;
        clicker.style.position = 'absolute';
        clicker.style.top = 0;
        btn.appendChild(clicker);

        return btn;
    }

    function updateCustomButton(node, oldModel, newModel) {
        var signal = newModel.signal;
        node.elm_up.elm_signal = signal;
        node.elm_hover.elm_signal = signal;
        node.elm_down.elm_signal = signal;

        var value = newModel.value;
        node.elm_up.elm_value = value;
        node.elm_hover.elm_value = value;
        node.elm_down.elm_value = value;

        Render.update(node.elm_up, oldModel.up, newModel.up)
        Render.update(node.elm_hover, oldModel.hover, newModel.hover)
        Render.update(node.elm_down, oldModel.down, newModel.down)
    }

    function max3(a,b,c) {
        var ab = a > b ? a : b;
        return ab > c ? ab : c;
    }

    function customButton(signal, value, up, hover, down) {
        return A3(newElement,
                  max3(up.props.width, hover.props.width, down.props.width),
                  max3(up.props.height, hover.props.height, down.props.height),
                  { ctor: 'Custom',
                    type: 'CustomButton',
                    render: renderCustomButton,
                    update: updateCustomButton,
                    model: { signal:signal, value:value, up:up, hover:hover, down:down }
                  });
    }

    function renderCheckbox(model) {
        var node = newNode('input');
        node.type = 'checkbox';
        node.checked = model.checked;
        node.style.display = 'block';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        function change() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.checked));
        }
        node.addEventListener('change', change);
        return node;
    }

    function updateCheckbox(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.checked = newModel.checked;
        return true;
    }

    function checkbox(signal, handler, checked) {
        return A3(newElement, 13, 13, {
            ctor: 'Custom',
            type: 'CheckBox',
            render: renderCheckbox,
            update: updateCheckbox,
            model: { signal:signal, handler:handler, checked:checked }
        });
    }

    function setRange(node, start, end, dir) {
        if (node.parentNode) {
            node.setSelectionRange(start, end, dir);
        } else {
            setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
        }
    }

    function renderField(model) {
        var field = newNode('input');
        field.elm_signal = model.signal;
        field.elm_handler = model.handler;

        field.type = model.type;
        field.placeholder = JS.fromString(model.placeHolder);
        field.value = JS.fromString(model.content.string);
        var selection = model.content.selection;
        var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
        setRange(field, selection.start, selection.end, direction);
        field.style.border = 'none';

        function update() {
            var direction = field.selectionDirection === 'backward' ? 'Backward' : 'Forward';
            elm.notify(field.elm_signal.id, field.elm_handler({
                _:{},
                string:JS.toString(field.value),
                selection: {
                    start:field.selectionStart,
                    end:field.selectionEnd,
                    direction: { ctor:direction }
                },
            }));
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

    function updateField(node, oldModel, newModel) {
        node.elmHandler = newModel.handler;
        var newStr = JS.fromString(newModel.content.string);
        if (node.value !== newStr) node.value = newStr;

        var selection = newModel.content.selection;
        var start = selection.start;
        var end = selection.end;
        var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
        if (end < start) {
            start = end;
            end = selection.start;
        }
        
        if (node.selectionStart !== start
            || node.selectionEnd !== end
            || node.selectionDirection !== direction) {
            setRange(node, start, end, direction);
        }
    }

    function mkField(type) {
        function field(signal, handler, placeHolder, content) {
            return A3(newElement, 200, 30, {
                ctor: 'Custom',
                type: type + 'Input',
                render: renderField,
                update: updateField,
                model: {
                    signal:signal,
                    handler:handler,
                    placeHolder:placeHolder,
                    content:content,
                    type:type
                }
            });
        }
        return F4(field);
    }


    function hoverable(signal, handler, element) {
        function onHover(bool) {
            elm.notify(signal.id, handler(bool));
        }
        var props = Utils.replace([['hover',onHover]], elem.props);
        return { props:props, element:elem.element };
    }

    function clickable(signal, value, element) {
        function onClick(bool) {
            elm.notify(signal.id, value);
        }
        var props = Utils.replace([['click',onClick]], elem.props);
        return { props:props, element:elem.element };
    }

    return elm.Native.Graphics.Input.values = {
        input:input,
        button:F3(button),
        customButton:F5(customButton),
        checkbox:F3(checkbox),
        dropDown:F2(dropDown),
        field:mkField('text'),
        email:mkField('email'),
        password:mkField('password'),
        hoverable:F3(hoverable),
        clickable:F3(clickable)
    };

};
