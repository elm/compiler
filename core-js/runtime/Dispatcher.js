
var Dispatcher = function() {
    var program = null;
    var inputs = [];
    var currentElement = null;

    var initialize = function() {
	program = Elm.main();
	if (!program.hasOwnProperty('recv')) {
	    program = Elm.Signal.constant(program);
	}

	currentElement = program.value;
	filterDeadInputs();

	var content = document.getElementById('content');
	content.appendChild(Render.render(currentElement));
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Elm.Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
	program = Elm.Signal.lift(function(value) {
		var content = document.getElementById('content');
		Render.update(content.firstChild,currentElement,value);
		currentElement = value;
		return value;
	    })(program);
    };
    var notify = function(id, v) {
	var timestep = (new window.Date).getTime();
	var hasListener = false;
	for (var i = inputs.length; i--; ) {
	    hasListener = inputs[i].recv(timestep, id, v) || hasListener;
	}
	return hasListener;
    };

    function isAlive(input) {
	if (!input.hasOwnProperty('defaultNumberOfKids')) return true;
	var len = input.kids.length;
	if (len == 0) return false;
	if (len > input.defaultNumberOfKids) return true;
	var alive = false;
	for (var i = len; i--; ) {
	    alive = alive || isAlive(input.kids[i]);
	}
	return alive;
    }
    function filterDeadInputs() {
	var temp = [];
	for (var i = inputs.length; i--; ) {
	    if (isAlive(inputs[i])) temp.push(inputs[i]);
	}
	inputs = temp;
    }

    return {initialize:initialize, notify:notify, inputs:inputs};
}();