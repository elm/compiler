Elm.Native.Graphics.Input = {};
Elm.Native.Graphics.Input.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.Input = elm.Native.Graphics.Input || {};
    if (elm.Native.Graphics.Input.values) return elm.Native.Graphics.Input.values;

    var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Text = Elm.Native.Text.make(elm);
    var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Tuple2 = Utils.Tuple2;

    function input(initialValue) {
        var signal = Signal.constant(initialValue);
        return { _:{}, signal:signal, handle:signal };
    }

    function renderDropDown(model) {
        var drop = newNode('select');
        drop.style.border = '0 solid';
        drop.style.pointerEvents = 'auto';
        drop.style.display = 'block';

        drop.elm_signal = model.signal;
        drop.elm_values = List.toArray(model.values);
        var values = drop.elm_values;

        for (var i = 0; i < values.length; ++i) {
            var option = newNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            drop.appendChild(option);
        }
        drop.addEventListener('change', function() {
            elm.notify(drop.elm_signal.id, drop.elm_values[drop.selectedIndex]._1);
        });

        return drop;
    }

    function updateDropDown(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_values = List.toArray(newModel.values);

        var values = node.elm_values;
        var kids = node.childNodes;
        var kidsLength = kids.length;

        var i = 0;
        for (; i < kidsLength && i < values.length; ++i) {
            var option = kids[i];
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
        }
        for (; i < kidsLength; ++i) {
            node.removeChild(node.lastChild);
        }
        for (; i < values.length; ++i) {
            var option = newNode('option');
            var name = values[i]._0;
            option.value = name;
            option.innerHTML = name;
            node.appendChild(option);
        }
    }

    function dropDown(signal, values) {
        return A3(newElement, 100, 24, {
            ctor: 'Custom',
            type: 'DropDown',
            render: renderDropDown,
            update: updateDropDown,
            model: {
                signal: signal,
                values: values
            }
        });
    }

    function renderSlider(model) {
        var node = newNode('input');
        node.type = 'range';

        node.min = model.min;
        node.max = model.max;
        node.step = model.step;
        node.value = model.val;

        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        node.addEventListener('input', function() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.value));
        });
        return node;
    }

    function updateSlider(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.min = newModel.min;
        node.max = newModel.max;
        node.step = newModel.step;
        node.value = newModel.val;
    }

    function slider(signal, handler, min, max, step, val) {
        return A3(newElement, 100, 24, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider,
            update: updateSlider,
            model: { signal:signal, handler:handler, min:min, max:max, step:step, val:val }
        });
    }

    function renderButton(model) {
        var node = newNode('button');
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
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
        node.elm_value = newModel.value;
        var txt = newModel.text;
        if (oldModel.text !== txt) node.innerHTML = txt;
    }

    function button(signal, value, text) {
        return A3(newElement, 100, 40, {
            ctor: 'Custom',
            type: 'Button',
            render: renderButton,
            update: updateButton,
            model: { signal:signal, value:value, text:text }
        });
    }

    function renderCustomButton(model) {
        var btn = newNode('div');
        btn.style.pointerEvents = 'auto';
        btn.elm_signal = model.signal;
        btn.elm_value = model.value;

        btn.elm_up    = Render.render(model.up);
        btn.elm_hover = Render.render(model.hover);
        btn.elm_down  = Render.render(model.down);

        btn.elm_up.style.display = 'block';
        btn.elm_hover.style.display = 'none';
        btn.elm_down.style.display = 'none';
  
        btn.appendChild(btn.elm_up);
        btn.appendChild(btn.elm_hover);
        btn.appendChild(btn.elm_down);

        function swap(visibleNode, hiddenNode1, hiddenNode2) {
            visibleNode.style.display = 'block';
            hiddenNode1.style.display = 'none';
            hiddenNode2.style.display = 'none';
        }

        var overCount = 0;
        function over(e) {
            if (overCount++ > 0) return;
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
        }
        function out(e) {
            if (btn.contains(e.toElement || e.relatedTarget)) return;
            overCount = 0;
            swap(btn.elm_up, btn.elm_down, btn.elm_hover);
        }
        function up() {
            swap(btn.elm_hover, btn.elm_down, btn.elm_up);
            elm.notify(btn.elm_signal.id, btn.elm_value);
        }
        function down() {
            swap(btn.elm_down, btn.elm_hover, btn.elm_up);
        }

        btn.addEventListener('mouseover', over);
        btn.addEventListener('mouseout' , out);
        btn.addEventListener('mousedown', down);
        btn.addEventListener('mouseup'  , up);

        return btn;
    }

    function updateCustomButton(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_value = newModel.value;

        var kids = node.childNodes;
        var styleUp    = kids[0].style.display;
        var styleHover = kids[1].style.display;
        var styleDown  = kids[2].style.display;

        Render.update(kids[0], oldModel.up, newModel.up);
        Render.update(kids[1], oldModel.hover, newModel.hover);
        Render.update(kids[2], oldModel.down, newModel.down);

        var kids = node.childNodes;
        kids[0].style.display = styleUp;
        kids[1].style.display = styleHover;
        kids[2].style.display = styleDown;
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
        node.style.pointerEvents = 'auto';
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

    function updateIfNeeded(css, attribute, latestAttribute) {
        if (css[attribute] !== latestAttribute) {
            css[attribute] = latestAttribute;
        }
    }
    function cssDimensions(dimensions) {
        return dimensions.top    + 'px ' +
               dimensions.right  + 'px ' +
               dimensions.bottom + 'px ' +
               dimensions.left   + 'px';
    }
    function updateFieldStyle(css, style) {
        updateIfNeeded(css, 'padding', cssDimensions(style.padding));

        var outline = style.outline;
        updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
        updateIfNeeded(css, 'border-color', toCss(outline.color));
        updateIfNeeded(css, 'border-radius', outline.radius + 'px');

        var highlight = style.highlight;
        if (highlight.width === 0) {
            css.outline = 'none';
        } else {
            updateIfNeeded(css, 'outline-width', highlight.width + 'px');
            updateIfNeeded(css, 'outline-color', toCss(highlight.color));
        }

        var textStyle = style.style;
        updateIfNeeded(css, 'color', toCss(textStyle.color));
        if (textStyle.typeface.ctor !== '[]') {
            updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
        }
        if (textStyle.height.ctor !== "Nothing") {
            updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
        }
        updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
        updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
        if (textStyle.line.ctor !== 'Nothing') {
            updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
        }
    }

    function renderField(model) {
        var field = newNode('input');
        updateFieldStyle(field.style, model.style);
        field.style.borderStyle = 'solid';
        field.style.pointerEvents = 'auto';

        field.type = model.type;
        field.placeholder = model.placeHolder;
        field.value = model.content.string;

        field.elm_signal = model.signal;
        field.elm_handler = model.handler;
        field.elm_old_value = field.value;

        function inputUpdate(event) {
            var curr = field.elm_old_value;
            var next = field.value;
            if (curr === next) {
                return;
            }

            var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
            var start = field.selectionStart;
            var end = field.selectionEnd;
            field.value = field.elm_old_value;

            elm.notify(field.elm_signal.id, field.elm_handler({
                _:{},
                string: next,
                selection: {
                    _:{},
                    start: start,
                    end: end,
                    direction: { ctor: direction }
                }
            }));
        }

        field.addEventListener('input', inputUpdate);
        field.addEventListener('focus', function() {
            field.elm_hasFocus = true;
        });
        field.addEventListener('blur', function() {
            field.elm_hasFocus = false;
        });

        return field;
    }

    function updateField(field, oldModel, newModel) {
        if (oldModel.style !== newModel.style) {
            updateFieldStyle(field.style, newModel.style);
        }
        field.elm_signal = newModel.signal;
        field.elm_handler = newModel.handler;

        field.type = newModel.type;
        field.placeholder = newModel.placeHolder;
        var value = newModel.content.string;
        field.value = value;
        field.elm_old_value = value;
        if (field.elm_hasFocus) {
            var selection = newModel.content.selection;
            var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
            setRange(field, selection.start, selection.end, direction);
        }
    }

    function mkField(type) {
        function field(style, signal, handler, placeHolder, content) {
            var padding = style.padding;
            var outline = style.outline.width;
            var adjustWidth = padding.left + padding.right + outline.left + outline.right;
            var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
            return A3(newElement, 200, 30, {
                ctor: 'Custom',
                type: type + 'Field',
                adjustWidth: adjustWidth,
                adjustHeight: adjustHeight,
                render: renderField,
                update: updateField,
                model: {
                    signal:signal,
                    handler:handler,
                    placeHolder:placeHolder,
                    content:content,
                    style:style,
                    type:type
                }
            });
        }
        return F5(field);
    }

    function hoverable(signal, handler, elem) {
        function onHover(bool) {
            elm.notify(signal.id, handler(bool));
        }
        var props = Utils.replace([['hover',onHover]], elem.props);
        return { props:props, element:elem.element };
    }

    function clickable(signal, value, elem) {
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
        slider:F6(slider),
        field:mkField('text'),
        email:mkField('email'),
        password:mkField('password'),
        hoverable:F3(hoverable),
        clickable:F3(clickable)
    };

};
