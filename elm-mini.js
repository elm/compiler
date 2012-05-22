
var Guid = function() {
 var counter = 0;
 var guid = function() { counter += 1; return counter; };
 return {guid : guid};
}();
var Value = function(){
  var Tuple = function() {
      var len = arguments.length;
      var arr = new Array(len+1);
      arr[0] = "Tuple" + arguments.length;
      for (var i = len; i--; ) {
	  arr[i+1] = arguments[i];
      }
      return arr;
  };

  var toString = function(v) {
    if (typeof v === "boolean") {
	return v ? "True" : "False";
    } else if (typeof v === "number") {
	return v+"";
    } else if (v[0]) {
	if (v[0].substring(0,5) === "Tuple") {
	    var output = "";
	    for (var i = v.length; --i; ) {
		output = "," + toString(v[i]) + output;
	    }
	    if (output[0] === ",") output = output.substring(1);
	    return "("+output+")";
	} else if (v[0] === "Cons") {
	    var start = (typeof v[1] === "string") ? '"' : "[";
	    var  end  = (typeof v[1] === "string") ? '"' : "]";
	    var  div  = (typeof v[1] === "string") ?  "" : ",";
	    var output = start + toString(v[1]);
	    v = v[2];
	    while (true) {
		if (v[0] === "Cons") {
		    output += div + toString(v[1]);
		    v = v[2];
		} else {
		    return output + end;
		}
	    }
	} else if (v[0] === "Nil") {
	    return "[]";
	} else {
	    var output = "";
	    for (var i = v.length; --i; ) {
		output = " " + toString(v[i]) + output
	    }
	    output = v[0] + output;
	    return (v.length > 1) ? "(" + output + ")" : output;
	}
    }
    return v+"";
  };
  var show = function(v) {
      return Text.monospace(String.properEscape(toString(v)));
  };
  var append = function(xs,ys) {
    if (typeof xs === "string" && typeof ys === "string") {
	return xs.concat(ys);
    }
    if (xs[0] === "Nil") {
	return ys;
    }
    var root = ["Cons", xs[1], ["Nil"]];
    var curr = root;
    xs = xs[2];
    while (xs[0]==="Cons") {
	curr[2] = ["Cons", xs[1], ["Nil"]];
	xs = xs[2];
	curr = curr[2];
    }
    curr[2] = ys;
    return root;
  };
  return {show:show, Tuple:Tuple, append:append};
}();var String = function() {
  var properEscape = function(str) {
      str.replace('"', "&#34;");
      str.replace("&", "&#38;");
      str.replace("'", "&#39;");
      str.replace("<", "&#60;");
      str.replace(">", "&#62;");
      return str;
  };

  var toText = function(elmList) {
    if (typeof elmList === "string") return elmList;
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return String.properEscape(a.join(''));
  };

  return {toText : toText,
	  properEscape : properEscape };
}();
var Color = function() {

 var create = function(r,g,b,a) {
    return { r: Math.round(255*r),
	     g: Math.round(255*g),
	     b: Math.round(255*b),
	     a: a };
 };

 var extract = function(c) {
    if (c.a === 1) {
	return 'rgb(' + c.r + ',' + c.g + ',' + c.b + ')';
    }
    return 'rgba(' + c.r + ',' + c.g + ',' + c.b + ',' + c.a + ')';
 };

 var rgba = function(r) { return function(g) { return function(b) {
	    return function(a) { return create(r,g,b,a); }; }; };
 };

 var rgb = function(r) { return function(g) { return function(b) {
	    return create(r,g,b,1); }; };
 };
 return {black: create(0,0,0,1),
	 white: create(1,1,1,1),
	 red  : create(1,0,0,1),
	 green: create(0,1,0,1),
	 blue : create(0,0,1,1),
	 rgba : rgba,
	 rgb  : rgb,
	 Internal : { extract:extract }
	 };
}();
var Element = function() {
  var newElement = function(elementType) {
    var e = document.createElement(elementType);    
    e.id = Guid.guid();
    //e.timesAdded = 0;
    return e;
  };
  var addTo = function(container, elem) {
      container.appendChild(clone(elem));
      //elem.timesAdded += 1;
  };
  var clone = function(e) {
    return e;
    /*if (e.timesAdded === 0) { return e; }
    if (e.tagName === "CANVAS") {
	var newCanvas = e.cloneNode(true);
	var ctx = newCanvas.getContext('2d');
	ctx.drawImage(e, 0, 0);
	return newCanvas;
    } else if (e.tagName === "IMG") {
	d = image([]);
	d.src = e.src;
	d.name = e.name;
	if (e.style.width !== "") { d.style.width = d.width = e.style.width; }
	if (e.style.height !== ""){d.style.height = d.height = e.style.height;}
	return d;
    } else if (e.hasOwnProperty('isElmLeaf')) {
	return e.cloneNode(true);
    } else {
	d = e.cloneNode(false);
	var kids = e.childNodes;
	var len = kids.length;
	for (var i = 0; i < len; i++) {
	    d.appendChild(clone(kids[i]));
	}
	return d;
	}*/
  };
  var divify = function(e) {
    var div = newElement('div');
    addTo(div, e);
    return div;
  };
  var makeText = function(w) { return function(pos) { return function(txt) {
	var e = newElement('div');
	e.isElmText = true;
	e.innerHTML = txt;
	e.style.textAlign = pos;
	if (w > 0) e.style.width = w + "px";
	e.isElmLeaf = true;
	e.style.visibility = "hidden";
	e.style.styleFloat = "left";
	e.style.cssFloat = "left";
	document.body.appendChild(e);
	var cStyle = window.getComputedStyle(e);
	if (w <= 0) e.style.width  = cStyle.getPropertyValue("width");
	e.style.height = cStyle.getPropertyValue("height");
	document.body.removeChild(e);
	e.style.visibility = "visible"
	e.style.styleFloat = "none";
	e.style.cssFloat = "none";
	return e;
      };
    };
  };
  var link = function (href) { return function (e) {
	  var a = newElement('a');
	  a.href = Text.fromString(href);
	  addTo(a,e);
	  return divify(a);
      };
  };

  var text = makeText(0)('left');
  var plainText = function(str) {
      return makeText(0)("left")(String.toText(str)); };
  var justifiedText = makeText(0)('justify');
  var centeredText = makeText(0)('center');
  var asText = function(v) { return makeText(0)("left")(Value.show(v)); };

  var image = function(src) {
    var img = newElement('img');
    img.isElmLeaf = true;
    img.onload = function() {
	if (img.style.width === "" && this.width > 0) {
	    img.style.width = img.width = this.width + "px";
	}
	if (img.style.height === "" && this.height > 0) {
	    img.style.height = img.height = this.height + "px";
	}
	Dispatcher.adjust()
    };
    img.src = String.toText(src);
    img.name = img.src;
    return img;
  };
  var video = function(src) {
    src = String.toText(src);
    var e = newElement('video');
    e.controls = "controls";
    var source = newElement('source');
    source.src = src;
    source.type = "video/" + src.substring(src.length - 3, src.length);
    addTo(e, source);
    e.isElmLeaf = true;
    return e;
  };
  var audio = function(src) {
    src = String.toString(src);
    var e = newElement('video');
    e.controls = "controls";
    var source = newElement('source');
    source.src = src;
    source.type = "audio/" + src.substring(src.length - 3, src.length);
    addTo(e, source);
    e.isElmLeaf = true;
    return e;
  };
  var collage = function(w) { return function(h) { return function(cflist) {
	    var canvas = newElement('canvas');
	    canvas.style.width  = w + 'px';
	    canvas.style.height = h + 'px';
	    canvas.width  = w;
	    canvas.height = h;
	    if (canvas.getContext) {
		var ctx = canvas.getContext('2d');
		ctx.clearRect(0,0, canvas.width, canvas.height);
		while (cflist[0] === "Cons") {
		    ctx = cflist[1](ctx);
		    cflist = cflist[2];
		}
		return canvas;
	    }
	    canvas.innerHTML = "Your browser does not support the canvas element.";
	    canvas.isElmLeaf = true;
	    return canvas;
	};
    };
  };

  var goDown = function(e) {
      return e.tagName === "DIV" ? e : divify(e);
  };
  var goRight = function(e) {
      e.style.styleFloat = "left";
      e.style.cssFloat = "left";
      return e;
  };
  var goIn = function(e) {
      e.style.position = 'absolute';
      return e;
  };
  var flowWith = function(dir, f, elist) {
      var container = newElement('div');
      for (var i = elist.length; i--; ) {
	  addTo(container, f(elist[i]));
      }
      container.elmFlowDirection = dir;
      return container;
  };

  var flow = function(direction) { return function(elist) {
	  var arr = [];
	  while (elist[0] === "Cons") {
	      arr.push(elist[1]);
	      elist = elist[2];
	  }
	  if (direction >= 3) arr.reverse();
	  var f = function(x) { return x; };
	  var dir = direction % 3;
	  if (dir == 0) return flowWith("Y", goDown , arr);
	  if (dir == 1) return flowWith("X", goRight, arr);
	  if (dir == 2) return flowWith("Z", goIn   , arr);
      };
  };

  var beside = function(a) { return function(b) {
	  return flow(1)(["Cons",a,["Cons",b,["Nil"]]]); }; };
  var above = function(a) { return function(b) {
	  return flow(0)(["Cons",a,["Cons",b,["Nil"]]]); }; };
  var below = function(a) { return function(b) {
	  return flow(3)(["Cons",a,["Cons",b,["Nil"]]]); }; };

  var box = function(pos) { return function(e) {
	  e.style.position = "absolute";
	  e.style.margin = "auto";
	  var x = (pos - 1) % 3;
	  var y = (pos - 1) / 3;
	  if (x < 2) e.style.left = 0;
	  if (x > 0) e.style.right = 0;
	  if (y < 2) e.style.top = 0;
	  if (y > 0) e.style.bottom = 0;
	  var div = newElement('div');
	  div.style.position = "relative";
	  addTo(div,e);
	  return div;
      };
  };

  var width = function(w) { return function(e) {
	  if (e.tagName === "A") { 
	      width(w)(e.firstChild);
	      return e;
	  }
	  if (e.hasOwnProperty('isElmText')) {
	      var d = makeText(w)(e.style.textAlign)(e.innerHTML);
	      e.style.height = d.style.height;
	  }
	  e.style.width = w + "px";
	  return e;
    };
  };
  var height = function(h) { return function(e) {
	(e.tagName === "A" ? e.firstChild : e).style.height = h + "px";
	return e;
    };
  };
  var size = function(w) { return function(h) { return function(e) {
	    var d = e.tagName === "A" ? e.firstChild : e;
	    d.style.width = w + "px";
	    d.style.height = h + "px";
	    return e;
	};
    };
  };

  var color = function(c) { return function(e) {
	e.style.backgroundColor = Color.Internal.extract(c);
	return e;
    };
  };
  var opacity = function(value) { return function(e) {
        e.style.opacity = value;
	return e;
    };
  };

  return {text : text,
	  image : image,
	  video : video,
	  audio : audio,
	  collage : collage,
	  flow : flow,
	  layers : flow(2),

	  beside : beside,
	  above : above,
	  below : below,
	  box : box,

	  width : width,
	  height : height,
	  size : size,
	  color : color,
	  opacity : opacity,
	  
	  link : link,
	  asText : asText,
	  plainText : plainText,
	  justifiedText : justifiedText,
	  centeredText : centeredText,

	  // directions
	  up : 0,
	  left : 1,
	  inward : 2,
	  down : 3,
	  right : 4,
	  outward : 5
	  };
}();
	  
	  

var Text = function() {
  var fromString = function(elmList) {
    if (typeof elmList === "string") return elmList;
    var a = [];
    while (elmList[0] === "Cons") {
      a.push(elmList[1]);
      elmList = elmList[2];
    }
    return String.properEscape(a.join(''));
  };

  var addTag = function(tag) { return function(text) {
	return '<' + tag + '>' + text + '</' + tag + '>';
    };
  };
  var addStyle = function(style, value) { return function(text) {
	return "<span style='" + style + ":" + value + "'>" + text + "</span>";
    };
  };

  var typeface = function(name) { return addStyle('font-family', name); };
  var size = function(px) {
    return addStyle('font-size', px + 'px');
  };
  var header = addTag('h1');
  var height = function(h) { return addStyle('font-size', h+'em'); }
  var italic = addStyle('font-style', 'italic');
  var bold = addTag('b');
  var color = function(c) {
    return addStyle('color', Color.Internal.extract(c));
  };
  var underline = addStyle('text-decoration', 'underline');
  var overline = addStyle('text-decoration', 'overline');
  var strikeThrough = addStyle('text-decoration', 'line-through');
  var link = function(href) { return function(text) {
      return "<a href='" + fromString(href) + "'>" + text + "</a>";
    };
  };

  return {fromString : fromString,
	  toText: fromString,
	  header : header,
	  height : height,
	  italic : italic,
	  bold : bold,
	  underline : underline,
	  overline : overline,
	  strikeThrough : strikeThrough,
	  monospace : typeface('monospace'),
	  color : color,
	  link : link };
}();
var Shape = function() {

  var create = function(center, ps, theta, s) {
    return { center: center, points: ps, theta: theta, scale: s };
  };

  ////  Construction  ////

  var polygon = function(ps) { return function(center) {
	var parr = [];
	while (ps[0] === "Cons") {
	    parr.push([ps[1][1], ps[1][2]]);
	    ps = ps[2];
	}
	center = [center[1], center[2]];
	return create(center, parr, 0, 1);
    };
};
  var ngon = function(n) { return function(r) { return function(center) {
	    var ps = [];
	    for (var i = n; i--;) {
		ps.push([r * Math.cos(Math.PI * 2 * i/n),
			 r * Math.sin(Math.PI * 2 * i/n)]);
	    }
	    center = [center[1], center[2]];
	    return create(center, ps, 0, 1);
	};
    };
};
  var rect = function(w) { return function(h) { return function(center) {
	    var ps = [[- w/2, - h/2],
		      [  w/2, - h/2],
		      [  w/2,   h/2],
		      [- w/2,   h/2]];
	    center = [center[1], center[2]];
	    return create(center, ps, 0, 1);
	};
    };
  };
  var oval = function(w) { return function(h) { return function(center) {
	    var ps = [];
	    for (var theta = 2 * Math.PI; theta > 0; theta -= Math.PI /50) {
		ps.push([w/2 * Math.cos(theta), h/2 * Math.sin(theta)]);
	    }
	    center = [center[1], center[2]];
	    return create(center, ps, 0, 1);
	};
    };
  };

////  Transforms  ////

  var move = function(x) { return function(y) { return function(shape) {
	var newCenter = [x + shape.center[0],y + shape.center[1]];
	return create(newCenter, shape.points, shape.theta, shape.scale);
      };
    };
  };
  var rotate = function(theta) { return function(shape) {
      var newTheta = shape.theta + 2 * Math.PI * theta;
      return create(shape.center, shape.points, newTheta, shape.scale);
    };
  };
  var scale = function(s) { return function(shape) {
      return create(shape.center, shape.points, shape.theta, shape.scale * s);
    };
  };


  ////  Atomize  ////

  var draw = function(fill) {
    return function(color) { return function(shape) { return function(ctx) {
		ctx.save();
		ctx.translate(shape.center[0], shape.center[1]);
		ctx.rotate(shape.theta);
		ctx.scale(shape.scale, shape.scale);
		ctx.beginPath();
		var points = shape.points;
	        ctx.moveTo(points[0][0], points[0][1]);
		for (var i = points.length; i--; ) {
		    ctx.lineTo(points[i][0], points[i][1]);
		}
		ctx.closePath();
		if (fill) {
		    ctx.fillStyle = Color.Internal.extract(color);
		    ctx.fill();
		} else {
		    ctx.strokeStyle = Color.Internal.extract(color);
		    ctx.stroke();
		}
		ctx.restore();
		return ctx;
	    };
	};
    };
  };

  var customOutline = function(p) {
    return function(c) { return function(shape) {
	    shape.points.push(shape.points[0]);
	    return Line.customLine(p)(c)(shape);
	};
    };
  };
  return {polygon:polygon, ngon:ngon, rect:rect, oval:oval,
	  move:move, rotate:rotate, scale:scale,
	  filled:draw(true), outlined:draw(false), customOutline:customOutline };
}();
var Line = function() {

  var pair = function(a,b) { return [a,b]; };

  var create = function(center, ps, theta, s) {
    return { center: center, points: ps, theta: theta, scale: s };
  };


  ////  Construction  ////

  var line = function(ps) {
    var parr = [];
    while (ps[0] === "Cons") {
	parr.push(pair(ps[1][1], ps[1][2]));
	ps = ps[2];
    }
    return create(pair(0,0), parr, 0, 1);
  };

  ////  Atomize  ////

  var solid = function(color) { return function(line) { return function(ctx) {
	    ctx.save();
	    ctx.beginPath();
	    ctx.translate(line.center[0], line.center[1]);
	    ctx.rotate(line.theta);
	    ctx.scale(line.scale, line.scale);
	    var points = line.points;
	    var i = points.length;
	    ctx.moveTo(points[i-1][0], points[i-1][1]);
	    while (i--) {
		ctx.lineTo(points[i][0], points[i][1]);
	    }
	    ctx.strokeStyle = Color.Internal.extract(color);
	    ctx.stroke();
	    ctx.restore();
	    return ctx;
	};
    };
  };

  var customLine = function(pattern) {
    return function(color) { return function(line) {
	    if (typeof pattern[0] === "string") {
		var temp = [];
		while (pattern[0] === "Cons") {
		    temp.push(pattern[1]);
		    pattern = pattern[2];
		}
		pattern = temp;
	    }
	    if (pattern.length === 0) { pattern = [8,4]; }
	    return function(ctx) {
		ctx.save();
		ctx.beginPath();
		ctx.translate(line.center[0], line.center[1]);
		ctx.rotate(line.theta);
		ctx.scale(line.scale, line.scale);
		customLineHelp(ctx, pattern, line.points);
		ctx.strokeStyle = Color.Internal.extract(color);
		ctx.stroke();
		ctx.restore();
		return ctx;
	    };
	};
    };
  };
  var customLineHelp = function(ctx, pattern, points) {
      var i = points.length - 1;
      var x0 = points[i][0], y0 = points[i][1];
      var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
      var pindex = 0, plen = pattern.length;
      var draw = true, segmentLength = pattern[0];
      ctx.moveTo(x0,y0);
      while (i--) {
	  x1 = points[i][0]; y1 = points[i][1];
	  dx = x1 - x0; dy = y1 - y0;
	  remaining = Math.sqrt(dx * dx + dy * dy);
	  while (segmentLength <= remaining) {
	      x0 += dx * segmentLength / remaining;
	      y0 += dy * segmentLength / remaining;
	      ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
	      // update starting position
	      dx = x1 - x0; dy = y1 - y0;
	      remaining = Math.sqrt(dx * dx + dy * dy);
	      // update pattern
	      draw = !draw;
	      pindex = (pindex + 1) % plen;
	      segmentLength = pattern[pindex];
	  }
	  if (remaining > 0) {
	      ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
	      segmentLength -= remaining;
	  }
	  x0 = x1; y0 = y1;
      }
  };

  return {line:line, customLine:customLine, solid:solid,
	  dashed: customLine([8,4]), dotted: customLine([3,3]) };
}();

var Elm = function() {
    var input = function(base) {
	this.id = Guid.guid();
	this.value = base;
	this.step = function(eid,v) {
	    var changed = eid === this.id;
	    if (changed) { this.value = v; }
	    return changed;
	};
    };
    var lift = function(func,args) {
	this.id = Guid.guid();
	this.value = null;

	args.reverse();
	this.recalc = function() {
	    var f = func;
	    for (var i = args.length; i--; ) {
		f = f(args[i].value);
	    }
	    this.value = f;
	};
	this.recalc();

	this.step = function(id,v) {
	    if (this.hasOwnProperty(id)) return false;
	    var changed = false;
	    for (var i = args.length; i--; ) {
		// Must not short-circuit!
		// All arguments need to be steped forward!
		changed = args[i].step(id,v) || changed;
	    }
	    changed ? (this.recalc()) : (this[id] = true);
	    return changed;
	};
    };
    var fold = function(func,base,input) {
	this.id = Guid.guid();
	this.value = base;
	this.step = function(id,v) {
	    if (this.hasOwnProperty(id)) return false;
	    var changed = input.step(id,v);
	    if (changed) { this.value = func(input.value)(this.value); }
	    else { this[id] = true; }
	    return changed;
	};
    };
    return {Input: function(x) {return new input(x);},
	    Lift:  function(f,xs){return new lift(f,xs);},
	    Fold:  function(f,b,x){return new fold(f,b,x);}
    };
}();

var Dispatcher = function() {
    var program = null;
    var correctSize = function(e) {
	var kids = e.childNodes;
	var len = kids.length;
	if (e.hasOwnProperty('isElmLeaf')) {
	    var w = e.style.width === "" ?
		0 : e.style.width.slice(0,-2) - 0;
	    var h = e.style.height === "" ?
		0 : e.style.height.slice(0,-2) - 0;
	    return [w, h];
	}
	if (len === 1) {
	    var dim = correctSize(kids[0]);
	    if (e.style.width !== "") { dim[0] = e.style.width.slice(0,-2) - 0; }
	    if (e.style.height !== "") { dim[1] = e.style.height.slice(0,-2) - 0; }
	    if (dim[0] !== 0) { e.style.width = dim[0] + "px"; }
	    if (dim[1] !== 0) { e.style.height = dim[1] + "px"; }
	    return dim;
	}
	var wmax = 0, hmax = 0, wsum = 0, hsum = 0;
	var hasWidth = true, hasHeight = true, dim = null;
	while (len--) {
	    dim = correctSize(kids[len]);
	    wmax = Math.max(wmax, dim[0]);
	    hmax = Math.max(hmax, dim[1]);
	    wsum += dim[0];
	    hsum += dim[1];
	    hasWidth  = hasWidth  && dim[0] > 0;
	    hasHeight = hasHeight && dim[1] > 0;
	}
	var w = wmax, h = hmax, dir = e.elmFlowDirection;
	if (dir === "X") { w = hasWidth ? wsum : 0; }
	if (dir === "Y") { h = hasHeight ? hsum : 0; }
	if (w > 0) e.style.width  = w + "px";
	if (h > 0) e.style.height = h + "px";
	return [w,h];
    };

    var initialize = function() {
	try { program = main(); } catch (e) {
	    document.body.innerHTML = "An Error Occured: Better Messages to come.";
	    throw e;
	}
	if (!program.hasOwnProperty('step')) {
	    program = Elm.Input(program);
	}
	var content = document.getElementById('content');
	content.appendChild(program.value);
	correctSize(content);
	var w = document.getElementById('widthChecker').offsetWidth;
	if (w !== window.innerWidth) {
	    Dispatcher.notify(Window.dimensions.id, Value.Tuple(w, window.innerHeight));
	}
    };
    var adjust = function() {
	var content = document.getElementById('content');
	correctSize(content);
    }
    var notify = function(id, v) {
	if (program.step(id,v)) {
	    var content = document.getElementById('content');
	    content.replaceChild(program.value, content.children[0]);
	    correctSize(content);
	}
    };
    return {initialize:initialize, notify:notify, adjust:adjust};
}();
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
    var clickedOn = function(elem) {
	var click = Elm.Input(false);
	addListener(elem, 'click', function(e) {
		Dispatcher.notify(click.id, true);
		Dispatcher.notify(click.id, false);
	    });
	return Value.Tuple(elem, click);
    };
    return {position: position,
	    x: Elm.Lift(function(p){return p[1];},[position]),
	    y: Elm.Lift(function(p){return p[2];},[position]),
	    isClicked: isClicked,
	    isDown: isDown,
	    clickedOn: clickedOn
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
}();var List = function() {

    var throwError = function() { throw "Function expecting a list!"; }

    function length(xs) {
	var out = 0;
	while (xs[0] === "Cons") {
	    out += 1;
	    xs = xs[2];
	}
	return out;
    };
    var reverse = foldl(function(x_72) {
      return function(y_73) {
        return["Cons", x_72, y_73]
      }
    })(["Nil"]);
    var concat = foldr(function(x_74) {
      return function(y_75) {
        return Value.append(x_74, y_75)
      }
    })(["Nil"]);
    var and = foldl(function(x_77) {
      return function(y_78) {
        return x_77 && y_78
      }
    })(true);
    var or = foldl(function(x_79) {
      return function(y_80) {
        return x_79 || y_80
      }
    })(false);
    var sum = foldl(function(x_89) {
      return function(y_90) {
        return x_89 + y_90
      }
    })(0);
    var product = foldl(function(x_91) {
      return function(y_92) {
        return x_91 * y_92
      }
    })(1);
    var maximum = foldl1(function(x) { return function(y) { return Math.max(x,y) } });
    var minimum = foldl1(function(x) { return function(y) { return Math.min(x,y) } });
    function head(v) {
	if (v[0] !== "Cons") {
	    throw "Error: 'head' only accepts lists of length greater than one.";
	}
	return v[1];
    }
    function tail(v) {
	if (v[0] !== "Cons") {
	    throw "Error: 'tail' only accepts lists of length greater than one.";
	}
	return v[2];
    }
    function map(f) {
      return function(xs) {
	  if (xs[0] === "Nil") { return xs; }
	  if (xs[0] !== "Cons") { throwError(); }
	  var root = ["Cons", f(xs[1]), ["Nil"]];
	  var curr = root;
	  xs = xs[2];
	  while (xs[0]==="Cons") {
	      curr[2] = ["Cons", f(xs[1]), ["Nil"]];
	      xs = xs[2];
	      curr = curr[2];
	  }
	  return root;
      }
    }
    function foldl(f) {
      return function(b) {
        return function(xs) {
          var acc = b;
	  if (xs[0] === "Nil") { return acc; }
	  if (xs[0] !== "Cons") { throwError(); }
	  while (xs[0] === "Cons") {
	      acc = f(xs[1])(acc);
	      xs = xs[2];
	  }
	  return acc;
        }
      }
    }
    function foldr(f) {
      return function(b) {
        return function(xs) {
          var acc = b;
	  if (xs[0] === "Nil") { return acc; }
	  if (xs[0] !== "Cons") { throwError(); }
	  var arr = [];
	  while (xs[0] === "Cons") {
	      arr.push(xs[1]);
	      xs = xs[2];
	  }
	  for (var i = arr.length; i--; ) {
	      acc = f(arr[i])(acc);
	  }
	  return acc;
        }
      }
    }
    function foldl1(f_49) {
      return function(_temp_50) {
        return function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var x_51 = v[1];
            var xs_52 = v[2];
            return foldl(f_49)(x_51)(xs_52)
          }
        }(_temp_50)
      }
    }
    function foldr1(f_53) {
      return function(_temp_54) {
        return function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var x_55 = v[1];
            var xs_56 = v[2];
            return foldr(f_53)(x_55)(xs_56)
          }
        }(_temp_54)
      }
    }
    function scanl(f) {
      return function(b) {
        return function(xs) {
          if (xs[0] === "Nil") { return ["Cons",b,["Nil"]]; }
	  if (xs[0] !== "Cons") { throwError(); }
	  var arr = [b];
	  while (xs[0] === "Cons") {
	      b = f(xs[1])(b);
	      arr.push(b);
	      xs = xs[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
        }
      }
    }
    function scanl1(f) {
      return function(xs) {
	if (xs[0] !== "Cons") {
	    throw "Error: 'scanl1' requires a list of at least length 1.";
	}
	return scanl(f)(xs[1])(xs[2]);
      }
    }
    function filter(pred) {
      return function(xs) {
        if (xs[0] === "Nil") { return xs; }
	if (xs[0] !== "Cons") { throwError(); }
	var arr = [];
	while (xs[0] === "Cons") {
          if (pred(xs[1])) { arr.push(xs[1]); }
	  xs = xs[2];
	}
	var out = ["Nil"];
	for (var i = arr.length; i--; ) {
	    out = ["Cons", arr[i], out];
	}
	return out;
      }
    }
    function concatMap(f_76) {
      return function(x) {
        return concat(map(f_76)(x))
      }
    }
    function forall(pred) {
	return foldl(function(x) { return function(acc) {
		    return acc && pred(x);
		};})(true);
    }
    function exists(pred) {
	return foldl(function(x) { return function(acc) {
		    return acc || pred(x);
		};})(false);
    }
    function partition(pred_93) {
      return function(lst_94) {
        return function() {
          var v = lst_94;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Tuple2", ["Nil"], ["Nil"]]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var x_95 = v[1];
              var xs_96 = v[2];
              return function(v) {
                if("Tuple2" !== v[0]) {
                  return undefined
                }else {
                  var as_97 = v[1];
                  var bs_98 = v[2];
                  return pred_93(x_95) ? ["Tuple2", ["Cons", x_95, as_97], bs_98] : ["Tuple2", as_97, ["Cons", x_95, bs_98]]
                }
              }(partition(pred_93)(xs_96))
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function zipWith(f) {
      return function(listA) {
	  return function(listB) {
	  if (listA[0] === "Nil" || listB[0] === "Nil") { return listA; }
	  if (listA[0] !== "Cons" || listB[0] !== "Cons") { throwError(); }
	  var arr = [];
	  while (listA[0] === "Cons" && listB[0] === "Cons") {
	      arr.push(f(listA[1])(listB[1]));
	      listA = listA[2];
	      listB = listB[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
        }
      }
    }
    function zip(listA) {
      return function(listB) {
	  if (listA[0] === "Nil" || listB[0] === "Nil") { return listA; }
	  if (listA[0] !== "Cons" || listB[0] !== "Cons") { throwError(); }
	  var arr = [];
	  while (listA[0] === "Cons" && listB[0] === "Cons") {
	      arr.push(["Tuple2", listA[1], listB[1]]);
	      listA = listA[2];
	      listB = listB[2];
	  }
	  var out = ["Nil"];
	  for (var i = arr.length; i--; ) {
	      out = ["Cons", arr[i], out];
	  }
	  return out;
      }
    }
    function unzip(pairs_112) {
      return function() {
        var v = pairs_112;
        var c = [function(v) {
          if("Nil" !== v[0]) {
            return undefined
          }else {
            return["Tuple2", ["Nil"], ["Nil"]]
          }
        }, function(v) {
          if("Cons" !== v[0]) {
            return undefined
          }else {
            var p_113 = v[1];
            var ps_114 = v[2];
            return function(v) {
              if("Tuple2" !== v[0]) {
                return undefined
              }else {
                if("Tuple2" !== v[1][0]) {
                  return undefined
                }else {
                  var x_115 = v[1][1];
                  var y_116 = v[1][2];
                  if("Tuple2" !== v[2][0]) {
                    return undefined
                  }else {
                    var xs_117 = v[2][1];
                    var ys_118 = v[2][2];
                    return["Tuple2", ["Cons", x_115, xs_117], ["Cons", y_116, ys_118]]
                  }
                }
              }
            }(["Tuple2", p_113, unzip(ps_114)])
          }
        }];
        for(var i = c.length;i--;) {
          var r = c[i](v);
          if(r !== undefined) {
            return r
          }
        }
      }()
    }
    function intersperse(sep_119) {
      return function(xs_120) {
        return function() {
          var v = xs_120;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Nil"]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_124 = v[1];
              if("Nil" !== v[2][0]) {
                return undefined
              }else {
                return["Cons", a_124, ["Nil"]]
              }
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_121 = v[1];
              if("Cons" !== v[2][0]) {
                return undefined
              }else {
                var b_122 = v[2][1];
                var cs_123 = v[2][2];
                return["Cons", a_121, ["Cons", sep_119, intersperse(sep_119)(["Cons", b_122, cs_123])]]
              }
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function intercalate(sep_125) {
      return function(xs_126) {
        return function() {
          var v = xs_126;
          var c = [function(v) {
            if("Nil" !== v[0]) {
              return undefined
            }else {
              return["Nil"]
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_130 = v[1];
              if("Nil" !== v[2][0]) {
                return undefined
              }else {
                return a_130
              }
            }
          }, function(v) {
            if("Cons" !== v[0]) {
              return undefined
            }else {
              var a_127 = v[1];
              if("Cons" !== v[2][0]) {
                return undefined
              }else {
                var b_128 = v[2][1];
                var cs_129 = v[2][2];
                return Value.append(a_127, Value.append(sep_125, intercalate(sep_125)(["Cons", b_128, cs_129])))
              }
            }
          }];
          for(var i = c.length;i--;) {
            var r = c[i](v);
            if(r !== undefined) {
              return r
            }
          }
        }()
      }
    }
    function sort(xs) {
      if (xs[0] === "Nil") { return xs; }
      if (xs[0] !== "Cons") { throwError(); }
      var arr = [];
      while (xs[0] === "Cons") {
	  arr.push(xs[1]);
	  xs = xs[2];
      }
      arr.sort(function(a,b) { return a - b});
      var out = ["Nil"];
      for (var i = arr.length; i--; ) {
	  out = [ "Cons", arr[i], out ];
      }
      return out;
    }
    return {head:head,
	    tail:tail,
	    map:map,
	    foldl:foldl,
	    foldr:foldr,
	    foldl1:foldl1,
	    foldr1:foldr1,
	    scanl:scanl,
	    scanl1:scanl1,
	    filter:filter,
	    length:length,
	    reverse:reverse,
	    concat:concat,
	    concatMap:concatMap,
	    and:and,
	    or:or,
	    forall:forall,
	    exists:exists,
	    sum:sum,
	    product:product,
	    maximum:maximum,
	    minimum:minimum,
	    partition:partition,
	    zipWith:zipWith,
	    zip:zip,
	    unzip:unzip,
	    intersperse:intersperse,
	    intercalate:intercalate,
	    sort:sort};
}();
var id  = function(x) { return x; };
var not = function(exp) { return !exp; };

var sqrt = function(x) { return Math.sqrt(x); };
var rem = function(x) { return function(y) { return x % y; }; };
var mod = function(x) { return function(y) {
  var r = x % y;
  var m = x==0 ? 0 : (y>0 ? (x>=0 ? r : r+y) : -mod(-x)(-y));
  return m == y ? 0 : m;
}; };
var abs = function(x) { return Math.abs(x); };
var logBase = function(base) { return function(x) { return Math.log(x) / Math.log(base); }; };

var min = function(x) { return function(y) { return Math.min(x,y); }; };
var max = function(x) { return function(y) { return Math.max(x,y); }; };
var clamp = function(lo) { return function(hi) {
	return function(x) { return Math.min(hi, Math.max(lo, x)); }; 
    };
};

var sin = Math.sin, cos = Math.cos, tan = Math.tan;
var asin = Math.asin, acos = Math.acos, atan = Math.atan;

var flip = function(f){return function(x){return function(y){return f(y)(x);};};};

var Just = function(x) { return ["Just",x]; };
var Nothing = ["Nothing"];

function constant(v) { return Elm.Input(v); }
function lift(f) { return function(e) { return Elm.Lift(f, [e]); }; }
function lift2(f) { return function(e1) { return function(e2) {
	    return Elm.Lift(f, [e1,e2]); }; }; }
function lift3(f) { return function(e1) { return function(e2) {
	    return function(e3) { return Elm.Lift(f, [e1,e2,e3]); }; }; }; }
function lift4(f) { return function(e1) { return function(e2) {
	    return function(e3) { return function(e4) {
		    return Elm.Lift(f, [e1,e2,e3,e4]); }; }; }; }; }
function foldp(f) { return function(b) { return function(e) {
	    return Elm.Fold(f,b,e); }; }; }


var includeGlobal = this;
(function() {
  var include = function(library) {
    for (var i in library) {
	if (i === 'Internal') continue;
	try {
	    includeGlobal[i] = library[i];
	} catch (err) {
	    if (i === 'length') {
		includeGlobal.execScript('var length;');
		length = library[i];
		continue;
	    }
	}
    }
  };
  var includeAs = function(name) { return function(library) {
	includeGlobal[name] = includeGlobal[name] || {};
	for (var i in library) {
	    if (i === 'Internal') continue;
	    includeGlobal[name][i] = library[i];
	}
    };
  };
  include (Element);
  include (Text);

  color = Element.color;
  height = Element.height;
  show = Value.show;

  includeAs ('Time')   (Signal.Time);
  includeAs ('Mouse')  (Signal.Mouse);
  includeAs ('Window') (Signal.Window);
  includeAs ('HTTP')   (Signal.HTTP);
  includeAs ('Input')  (Signal.Input);
  includeAs ('Random') (Signal.Random);
  
  include (Color);
  include (Shape);
  include (Line);
}());