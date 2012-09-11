
var Signal = function() {
  function toElmString(str) {
      var out = ["Nil"];
      for (var i = str.length; i--; ) {
	  out = ["Cons", str[i], out];
      }
      return out;
  }
  var addListener = function() {
	  if(document.addEventListener) {
	      return function(element, event, handler) {
		  element.addEventListener(event, handler, false);
	      };
	  }
	  else {
	      return function(element, event, handler) {
		  element.attachEvent('on' + event, handler);
	      };
	  }
      }();

  var Mouse = function() {
    var position  = Elm.Input(Value.Tuple(0,0));
    position.defaultNumberOfKids = 2;

    var x = Elm.Lift(function(p){return p[1];},[position]);
    x.defaultNumberOfKids = 0;
    var y = Elm.Lift(function(p){return p[2];},[position]);
    y.defaultNumberOfKids = 0;

    var isDown    = Elm.Input(false);
    var isClicked = Elm.Input(false);
    var clicks = Elm.Input(Value.Tuple());
    
    function getXY(e) {
      var posx = 0;
      var posy = 0;
      if (!e) var e = window.event;
      if (e.pageX || e.pageY) {
	posx = e.pageX;
	posy = e.pageY;
      } else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft +
	    document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop +
	    document.documentElement.scrollTop;
      }
      return Value.Tuple(posx, posy);
    }

    addListener(document, 'click', function(e) {
	    var hasListener1 = Dispatcher.notify(isClicked.id, true);
	    var hasListener2 = Dispatcher.notify(clicks.id, Value.Tuple());
	    Dispatcher.notify(isClicked.id, false);
	    if (!hasListener1 && !hasListener2)
		this.removeEventListener('click',arguments.callee,false);
	});
    addListener(document, 'mousedown', function(e) {
	    var hasListener = Dispatcher.notify(isDown.id, true);
	    if (!hasListener)
		this.removeEventListener('mousedown',arguments.callee,false);
	});
    addListener(document, 'mouseup', function(e) {
	    var hasListener = Dispatcher.notify(isDown.id, false);
	    if (!hasListener)
		this.removeEventListener('mouseup',arguments.callee,false);
	});
    addListener(document, 'mousemove', function(e) {
	    var hasListener = Dispatcher.notify(position.id, getXY(e));
	    if (!hasListener)
		this.removeEventListener('mousemove',arguments.callee,false);
	});
    var clickedOn = function(elem) {
	var click = Elm.Input(false);
	addListener(elem, 'click', function(e) {
		Dispatcher.notify(click.id, true);
		Dispatcher.notify(click.id, false);
	    });
	return Value.Tuple(elem, click);
    };
    return {position: position,
	    x:x,
	    y:y,
	    isClicked: isClicked,
	    isDown: isDown,
	    clicks: clicks,
	    isClickedOn: clickedOn
	    };
  }();

  var Time = function() {
      var every = function(t) {
	  t *= 1000;
	  var clock = Elm.Input(0);
	  var time = 0;
	  setInterval(function() {
		  time += t;
		  Dispatcher.notify(clock.id, time/1000);
	      }, t);
	  return clock;
      };
      var after = function(t) {
	  t *= 1000;
	  var thread = Elm.Input(false);
	  setTimeout(function() { Dispatcher.notify(thread.id, true); }, t);
	  return thread;
      };
      var before = function(t) {
	  t *= 1000;
	  var thread = Elm.Input(true);
	  setTimeout(function() { Dispatcher.notify(thread.id, false); }, t);
	  return thread;
      };
      return {every:every,after:after,before:before};
  }();

  var Window = function() {
    var dimensions = Elm.Input(Value.Tuple(window.innerWidth,window.innerHeight));
    dimensions.defaultNumberOfKids = 2;

    var width  = Elm.Lift(function(p){return p[1];},[dimensions]);
    width.defaultNumberOfKids = 0;
    var height = Elm.Lift(function(p){return p[2];},[dimensions]);
    height.defaultNumberOfKids = 0;

    addListener(window, 'resize', function(e) {
	    var w = document.getElementById('widthChecker').offsetWidth;
	    var hasListener = Dispatcher.notify(dimensions.id, Value.Tuple(w, window.innerHeight));
	    if (!hasListener)
		this.removeEventListener('resize',arguments.callee,false);
	});
    return {dimensions:dimensions,width:width,height:height};
  }();

  var Keyboard = { Raw : function() {
    var keysDown = Elm.Input(["Nil"]);
    var charPressed = Elm.Input(["Nothing"]);
    function remove(x,xs) {
	if (xs[0] === "Nil") return xs;
	if (xs[1] === x) return xs[2];
	return ["Cons", xs[1], remove(x,xs[2])];
    }
    function has(x,xs) {
	while (xs[0] !== "Nil") {
	    if (xs[1] === x) return true;
	    xs = xs[2];
	}
	return false;
    }
    addListener(document, 'keydown', function(e) {
	    if (has(e.keyCode, keysDown.value)) return;
	    var hasListener = Dispatcher.notify(keysDown.id, ["Cons", e.keyCode, keysDown.value]);
	    if (!hasListener)
		this.removeEventListener('keydown',arguments.callee,false);
	});
    addListener(document, 'keyup', function(e) {
	    var codes = remove(e.keyCode, keysDown.value);
	    var hasListener = Dispatcher.notify(keysDown.id, codes);
	    if (!hasListener)
		this.removeEventListener('keyup',arguments.callee,false);
	});
    addListener(window, 'blur', function(e) {
	    var hasListener = Dispatcher.notify(keysDown.id, ["Nil"]);
	    if (!hasListener)
		this.removeEventListener('blur',arguments.callee,false);
	});
    addListener(document, 'keypress', function(e) {
	    var hasListener = Dispatcher.notify(charPressed.id, ["Just",e.charCode || e.keyCode]);
	    Dispatcher.notify(charPressed.id, ["Nothing"]);
	    if (!hasListener)
		this.removeEventListener('keypress',arguments.callee,false);
	});
    return {keysDown:keysDown,
	    charPressed:charPressed};
      }()
  };

  var HTTP = function() {
      var fetch = function(how) { return function(url) {
	      var thread = Elm.Input(["Waiting"]);
	      var request = {};
	      if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
	      else if (window.ActiveXObject) { request = new ActiveXObject("Microsoft.XMLHTTP"); }
	      request.onreadystatechange = function(e) {
		  if (request.readyState === 4) {
		      Dispatcher.notify(thread.id,
					request.status === 200
					? ["Success", toElmString(request.responseText)]
					: ["Failure", request.status, toElmString(request.statusText)]);
		  }
	      };
	      request.open(how, Value.toText(url), true);
	      request.send(null);
	      return thread;
	  };
      };
      var fetches = function(how) { return function(input) {
	      var output = Elm.Input(["Nothing"]);
	      var fetcher = Elm.Lift(update, [input]);
	      var combine = Elm.Lift(function(x) { return function(y) { return x; } }, [output,fetcher]);
	      function update(strOpt) {
		  if (strOpt[0] !== "Just") {
		      try { Dispatcher.notify(output.id, ["Nothing"]); } catch(e) {}
		      return [];
		  }
		  try {
		      Dispatcher.notify(output.id, ["Just", ["Waiting"]]);
		  } catch(e) { output.value = ["Just", ["Waiting"]]; }
		  var request = {};
		  if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
		  else if (window.ActiveXObject) { request = new ActiveXObject("Microsoft.XMLHTTP"); }
		  request.onreadystatechange = function(e) {
		      if (request.readyState === 4) {
			  Dispatcher.notify(output.id,
					    ["Just", request.status === 200
					     ? ["Success", toElmString(request.responseText)]
					     : ["Failure", request.status, toElmString(request.statusText)]]);
		      }
		  };
		  request.open(how, Value.toText(strOpt[1]), true);
		  request.send(null);
		  return [];
	      }
	      return combine;
	  };
      };
      return {get : fetch("GET"), post : fetch("POST"),
	      gets : fetches("GET"), posts : fetches("POST")
	      };
  }();
  var Random = function() {
      var inRange = function(min) { return function(max) {
	      return Elm.Input(Math.floor(Math.random() * (max-min+1)) + min);
	  };
      };
      var randomize = function(min) { return function(max) { return function(signal) {
		  return Elm.Lift(function(x) { return Math.floor(Math.random() * (max-min+1)) + min;},[signal]);
	      };
	  };
      };
      return { inRange:inRange, randomize:randomize };
  }();
  var Input = function() {
      function wrap(elem) {
	  var p = Value.getSize(elem);
	  return ["Element", Guid.guid(), ["EHtml",elem], p[0], p[1], 1, Nothing, Nothing];
      }
      var newTextInput = function(elem, ghostText) {
	  elem.placeholder = Foreign.JavaScript.castStringToJSString(ghostText);
	  var str = Elm.Input(["Nil"]);
	  addListener(elem, 'keyup', function(e) {
		  Dispatcher.notify(str.id, toElmString(elem.value));
		  elem.focus();
	      });
	  elem.style.padding = "1px";
	  return Value.Tuple(wrap(elem), str);
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
	  var status = Elm.Input(checked);
	  addListener(box, 'change', function(e) {
		  Dispatcher.notify(status.id, box.checked);
	      });
	  return Value.Tuple(wrap(box), status);
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
	  var status = Elm.Input(opts[0]);
	  addListener(slct, 'change', function(e) {
		  Dispatcher.notify(status.id, opts[slct.selectedIndex]);
	      });
	  return Value.Tuple(wrap(slct), status);
      };
      var stringDropDown = function(opts) {
	  return dropDown(List.map (function(x) {return Value.Tuple(x,x);}) (opts));
      };
      var button = function(name) {
	  var b = newElement('input');
	  b.type = "button";
	  b.value = Foreign.JavaScript.castStringToJSString(name);
	  var press = Elm.Input(false);
	  addListener(b, 'click', function(e) {
		  Dispatcher.notify(press.id, true);
		  Dispatcher.notify(press.id, false);
	      });
	  return Value.Tuple(wrap(b),press);
      };
      return {textArea:textArea, textField:textField,
	      password:password, checkbox:checkbox,
	      dropDown:dropDown, stringDropDown:stringDropDown,
	      button:button};
  }();

  return {addListener:addListener,
	  Mouse:Mouse,
	  Keyboard:Keyboard,
	  Time:Time,
	  Window:Window,
	  HTTP:HTTP,
	  Random:Random,
	  Input:Input,
	  constant : function(v) { return Elm.Input(v); },
	  lift  : function(f){return function(e){return Elm.Lift(f,[e]);};},
	  lift2 : function(f) { return function(e1) { return function(e2) {
		  return Elm.Lift(f, [e1,e2]); }; }; },
	  lift3 : function(f) { return function(e1) { return function(e2) {
		  return function(e3){return Elm.Lift(f,[e1,e2,e3]);};};};},
	  lift4 : function(f) { return function(e1) { return function(e2) {
		  return function(e3) { return function(e4) {
			  return Elm.Lift(f, [e1,e2,e3,e4]); }; }; }; }; },
	  foldp : function(f) { return function(b) { return function(e) {
		  return Elm.Fold(f,b,e); }; }; },
	  count : function(sig){return Elm.Fold(function(_){return function(c){return c+1;};},0,sig)},
	  keepIf : Elm.keepIf,
	  dropIf : Elm.dropIf,
	  keepWhen : Elm.keepWhen,
	  dropWhen : Elm.dropWhen,
	  dropRepeats : Elm.dropRepeats,
	  sampleOn : Elm.sampleOn
  };
}();