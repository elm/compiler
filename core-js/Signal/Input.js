
Elm.Input = function() {
    var JS = Elm.JavaScript;
    var toElmString = Elm.JavaScript.castJSStringToString;
    var newTextInput = function(elem, ghostText) {
	elem.placeholder = JS.castStringToJSString(ghostText);
	var str = Elm.Signal.constant(["Nil"]);
	Value.addListener(elem, 'keyup', function(e) {
		Dispatcher.notify(str.id, toElmString(elem.value));
		elem.focus();
	    });
	elem.style.padding = "1px";
	return Value.Tuple(Value.wrap(elem), str);
    };
    var newElement = function(name) {
	var e = document.createElement(name);
	e.style.padding = "0";
	e.style.margin = "0";
	return e;
    };
    var textArea = function(cols) { return function(rows) {
	    var textarea = newElement('textarea');
	    textarea.rows = rows;
	    textarea.cols = cols;
	    return newTextInput(textarea, "");
	};
    };
    var textField = function(ghostText) {
	var field = newElement('input');
	field.type = 'text';
	return newTextInput(field, ghostText);
    };
    var password = function(ghostText) {
	var field = newElement('input');
	field.type = 'password';
	return newTextInput(field, ghostText);
    };
    var checkbox = function(checked) {
	var box = newElement('input');
	box.type = 'checkbox';
	box.checked = checked;
	var status = Elm.Signal.constant(checked);
	Value.addListener(box, 'change', function(e) {
		Dispatcher.notify(status.id, box.checked);
	    });
	return Value.Tuple(Value.wrap(box), status);
    };
    var dropDown = function(options) {
	var slct = newElement('select');
	var opts = [];
	while (options[0] === "Cons") {
	    var opt = newElement('option');
	    var str = Text.toText(options[1][1]);
	    opt.value = str;
	    opt.innerHTML = str;
	    slct.appendChild(opt);
	    opts.push(options[1][2]);
	    options = options[2];
	}
	var status = Elm.Signal.constant(opts[0]);
	Value.addListener(slct, 'change', function(e) {
		Dispatcher.notify(status.id, opts[slct.selectedIndex]);
	    });
	return Value.Tuple(Value.wrap(slct), status);
    };
    var stringDropDown = function(opts) {
	return dropDown(Elm.List.map (function(x) {return Value.Tuple(x,x);}) (opts));
    };
    var button = function(name) {
	var b = newElement('input');
	b.type = "button";
	b.value = JS.castStringToJSString(name);
	var press = Elm.Signal.constant(false);
	Value.addListener(b, 'click', function(e) {
		Dispatcher.notify(press.id, true);
		Dispatcher.notify(press.id, false);
	    });
	return Value.Tuple(Value.wrap(b),press);
    };
    return {textArea:textArea, textField:textField,
	    password:password, checkbox:checkbox,
	    dropDown:dropDown, stringDropDown:stringDropDown,
	    button:button};
}();
