
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
    var isDown    = Elm.Input(false);
    var isClicked = Elm.Input(false);
    
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
	    Dispatcher.notify(isClicked.id, true);
	    Dispatcher.notify(isClicked.id, false);
	});
    addListener(document, 'mousedown', function(e) {
	    Dispatcher.notify(isDown.id, true); });
    addListener(document, 'mouseup', function(e) {
	    Dispatcher.notify(isDown.id, false); });
    addListener(document, 'mousemove', function(e) {
	    Dispatcher.notify(position.id, getXY(e)); });
    return {position: position,
	    x: Elm.Lift(function(p){return p[1];},[position]),
	    y: Elm.Lift(function(p){return p[2];},[position]),
	    isClicked: isClicked,
	    isDown: isDown
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
    addListener(window, 'resize', function(e) {
	    var w = document.getElementById('widthChecker').offsetWidth;
	    Dispatcher.notify(dimensions.id, Value.Tuple(w, window.innerHeight));
	});
    return {dimensions:dimensions,
	    width : Elm.Lift(function(p){return p[1];},[dimensions]),
	    height: Elm.Lift(function(p){return p[2];},[dimensions]) };
  }();

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
	      request.open(how, String.toText(url), true);
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
		  request.open(how, String.toText(strOpt[1]), true);
		  request.send(null);
		  return [];
	      }
	      return combine;
	  };
      };
      return {get : fetch("GET"), post : fetch("POST"),
	      gets : fetches("GET"), posts : fetches("POST") };
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
      var newTextInput = function(elem, ghostText) {
	  elem.isElmLeaf = true;
	  var str = Elm.Input(["Nil"]);
	  addListener(elem, 'keyup', function(e) {
		  Dispatcher.notify(str.id, toElmString(elem.value));
		  elem.focus();
	      });
	  return Value.Tuple(elem, str);
      };
      var newElement = function(name) {
	  var e = document.createElement(name);
	  e.id = Guid.guid();
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
	  return Value.Tuple(box, status);
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
	  return Value.Tuple(slct, status);
      };
      var stringDropDown = function(opts) {
	  return dropDown(List.map (function(x) {return Value.Tuple(x,x);}) (opts));
      };
      var button = function(name) {
	  var b = newElement('input');
	  b.type = "button";
	  b.value = Text.toText(name);
	  var press = Elm.Input(false);
	  addListener(b, 'click', function(e) {
		  Dispatcher.notify(press.id, true);
		  Dispatcher.notify(press.id, false);
	      });
	  return Value.Tuple(b,press);
      };
      return {textArea:textArea, textField:textField,
	      password:password, checkbox:checkbox,
	      dropDown:dropDown, stringDropDown:stringDropDown,
	      button:button};
  }();

  return {Mouse:Mouse, Time:Time, Window:Window,
	  HTTP:HTTP, Random:Random, Input:Input };
}();