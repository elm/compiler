
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

  var rectangle = function(w) { return function(h) {
	  var e = newElement('div');
	  e.isElmLeaf = true;
	  e.style.width = w + "px";
	  e.style.height = h + "px";
	  return e;
      };
  };

  var makeText = function(w) { return function(pos) { return function(txt) {
	var e = newElement('div');
	e.isElmLeaf = true;
	e.isElmText = true;
	e.innerHTML = txt;
	e.style.textAlign = pos;
	if (w > 0) e.style.width = w + "px";
	return e;
      };
    };
  };
  var correctTextSize = function(e) {
    var w = e.style.width ? e.style.width.slice(0,-2) : 0;

    var t = newElement('div');
    t.innerHTML = "&nbsp;" + e.innerHTML;
    t.style.textAlign = e.style.textAlign;
    if (w > 0) { t.style.width = w + "px"; }
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat = "left";
    
    document.body.appendChild(t);
    var cStyle = window.getComputedStyle(t);
    if (w <= 0) e.style.width = cStyle.getPropertyValue("width");
    e.style.height = cStyle.getPropertyValue("height");
    document.body.removeChild(t);
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
      return makeText(0)("left")(Data.String.toText(str)); };
  var justifiedText = makeText(0)('justify');
  var centeredText = makeText(0)('center');
  var rightedText = makeText(0)('right');
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
    img.src = Data.String.toText(src);
    img.name = img.src;
    return img;
  };
  var fittedImage = function(w) { return function(h) { return function(src) {
        var canvas = newElement('canvas');
	canvas.style.width  = w + 'px';
	canvas.style.height = h + 'px';
	canvas.width  = w;
	canvas.height = h;
	canvas.innerHTML = "Your browser does not support the canvas element.";
	canvas.isElmLeaf = true;

        var img = newElement('img');
	img.onload = function() {
           if (canvas.getContext) {
	     var ctx = canvas.getContext('2d');
	     var sx = 0, sy = 0, sWidth = this.width, sHeight = this.height;
	     if (w / h > this.width / this.height) {
	       sHeight = this.width * h / w;
	       sy = (this.height - sHeight) / 2
	     } else {
               sWidth = this.height * w / h;
	       sx = (this.width - sWidth) / 2
	     }
	     ctx.drawImage(img, sx, sy, sWidth, sHeight,
			   0,0, canvas.width, canvas.height);
	   }
	};
	img.src = Data.String.toText(src);
	return canvas;
      };
    };
  };

  var video = function(src) {
    src = Data.String.toText(src);
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
    src = Data.String.toString(src);
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
	  return flow(4)(["Cons",a,["Cons",b,["Nil"]]]); }; };
  var above = function(a) { return function(b) {
	  return flow(3)(["Cons",a,["Cons",b,["Nil"]]]); }; };
  var below = function(a) { return function(b) {
	  return flow(0)(["Cons",a,["Cons",b,["Nil"]]]); }; };

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
	  fittedImage : fittedImage,
	  video : video,
	  audio : audio,
	  collage : collage,
	  flow : flow,
	  layers : flow(2),
	  rectangle : rectangle,

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
	  rightedText : rightedText,

	  // directions
	  up : 0,
	  left : 1,
	  inward : 2,
	  down : 3,
	  right : 4,
	  outward : 5,

	  correctTextSize : correctTextSize
	  };
}();
	  
	  
