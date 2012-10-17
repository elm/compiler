
var Render = function(){

function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
};

function addTo(container, elem) {
    container.appendChild(elem);
};

function makeText(pos,txt) {
    var e = newElement('div');
    e.innerHTML = txt;
    e.style.textAlign = pos;
    return e;
};

function image(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function fittedImage(w,h,src) {
    var canvas = newElement('canvas');
    canvas.style.display = "block";
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.width  = w;
    canvas.height = h;
    canvas.innerHTML = "Your browser does not support the canvas element.";

    var img = newElement('img');
    img.onload = function() {
	if (canvas.getContext) {
	    var ctx = canvas.getContext('2d');
	    var sx = 0, sy = 0, sWidth = this.width, sHeight = this.height;
	    if (w / h > this.width / this.height) {
		sHeight = this.width * h / w;
		sy = (this.height - sHeight) / 2;
	    } else {
		sWidth = this.height * w / h;
		sx = (this.width - sWidth) / 2;
	    }
	    ctx.drawImage(img, sx, sy, sWidth, sHeight,
			  0,0, canvas.width, canvas.height);
	}
    };
    img.src = src;
    return canvas;
};

var video = function(src) {
    var e = newElement('video');
    e.controls = "controls";
    var source = newElement('source');
    source.src = src;
    var segs = src.split('.');
    source.type = "video/" + segs[segs.length-1];
    addTo(e, source);
    e.style.display = "block";
    return e;
};

function divify(e) {
    var div = newElement('div');
    addTo(div, e);
    return div;
};
function goDown(e) {
    return e //.tagName === "DIV" ? e : divify(e);
};
function goRight(e) {
    e.style.styleFloat = "left";
    e.style.cssFloat = "left";
    return e;
};
function goIn(e) {
    e.style.position = 'absolute';
    return e;
};
function flowWith(f, prep, elist) {
    var container = newElement('div');
    for (var i = elist.length; i--; ) {
	addTo(container, f(prep(elist[i])));
    }
    return container;
};

function flow(dir,elist) {
    switch(dir) {
    case "DDown":  elist = elist.slice(0).reverse();
    case "DUp":    return flowWith(goDown,render,elist);
    case "DRight": elist = elist.slice(0).reverse();
    case "DLeft":  return flowWith(goRight,render,elist);
    case "DOut":   elist = elist.slice(0).reverse();
    case "DIn":    return flowWith(goIn,render,elist);
    };
};

function toPos(pos) {
    switch(pos[0]) {
    case "Absolute": return  pos[1] + "px";
    case "Relative": return (pos[1] * 100) + "%";
    }
}

function setPos(pos,e) {
  e.style.position = 'absolute';
  e.style.margin = 'auto';
  switch(pos[0]) {
  case "Position":
      if (pos[1][0] !== "Far")  e.style.left = 0;
      if (pos[1][0] !== "Near") e.style.right = 0;
      if (pos[2][0] !== "Far")  e.style.top = 0;
      if (pos[2][0] !== "Near") e.style.bottom = 0;
      break;
  case "PositionAt":
      e.style.top  = toPos(pos[2]);
      e.style.left = toPos(pos[1]);
      var shift = "translate(" + (-elem[3]/2) + "px," + (-elem[4]/2) + "px)";
      e.style.transform       = shift;
      e.style.msTransform     = shift;
      e.style.MozTransform    = shift;
      e.style.webkitTransform = shift;
      e.style.OTransform      = shift;
      break;
  default:
      var p = pos[0].slice(-2);
      e.style[p[0] === "T" ? 'top' : 'bottom'] = toPos(pos[2]);
      e.style[p[1] === "L" ? 'left' : 'right'] = toPos(pos[1]);
  }
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos,e);
    var div = newElement('div');
    div.style.position = "relative";
    div.style.overflow = "hidden";
    addTo(div,e);
    return div;
};

function render(elem) {
    var e = {};
    switch(elem[2][0]) {
    case "EText":        e = makeText(elem[2][1],elem[2][2]); break;
    case "EImage":       e = image(elem[2][1]); break;
    case "EVideo":       e = video(elem[2][1]); break;
    case "EFittedImage": e = fittedImage(elem[3],elem[4],elem[2][1]); break;
    case "EFlow":        e = flow(elem[2][1][0],elem[2][2]); break;
    case "ECollage":     e = Collage.collage(elem[2][1],elem[2][2],elem[2][3]); break;
    case "EEmpty":       e = newElement('div'); break;
    case "EContainer":   e = container(elem[2][1],elem[2][2]); break;
    case "EHtml":
	e = elem[2][1];
	if (e.type !== 'button') {
	    var p = Value.getExcess(e);
	    elem[3] -= p[0];
	    elem[4] -= p[1];
	}
	break;
    case "EExternalHtml":
	e = newElement('div');
	addTo(e, elem[2][1]);
	break;
    }
    e.id = elem[1];
    e.style.width  = (~~elem[3]) + 'px';
    e.style.height = (~~elem[4]) + 'px';
    if (elem[5] !== 1) { e.style.opacity = elem[5]; }
    if (elem[6][0] === "Just") {
	e.style.backgroundColor = Elm.Graphics.Color.extract(elem[6][1]);
    }
    if (elem[7][0] === "Just") {
	var a = newElement('a');
	a.href = elem[7][1];
	addTo(a,e);
	return a;
    }
    return e;
};

function update(node,curr,next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr[1] === next[1]) return;
    if (curr[2][0] !== next[2][0]) {
	return node.parentNode.replaceChild(render(next),node);
    }
    var nextE = next[2], currE = curr[2];
    switch(nextE[0]) {
    case "EText":
	if (nextE[1] !== currE[1]) node.style.textAlign = nextE[1];
	if (nextE[2] !== currE[2]) node.innerHTML = nextE[2];
	break;
    case "EImage":
	if (nextE[1] !== currE[1]) node.src = nextE[1];
	break;
    case "EVideo":
    case "EFittedImage":
	if (!Value.eq(nextE,currE) || next[3]!==curr[3] || next[4]!==curr[4]) {
	    return node.parentNode.replaceChild(render(next),node);
	}
    break;
    case "ECollage":
	if (nextE[1] !== currE[1] || nextE[2] !== currE[2] || nextE[3].length !== currE[3].length) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	Collage.updateCollage(node,currE[3],nextE[3]);
	break;
    case "EFlow":
	if (nextE[1] !== currE[1]) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	var nexts = nextE[2];
	var kids = node.childNodes;
	if (nexts.length !== kids.length) {
	    return node.parentNode.replaceChild(render(next),node);
	}
	var currs = currE[2];
	var goDir = {};
	switch(nextE[1][0]) {
	case "DDown":  case "DUp":   goDir = goDown; break;
	case "DRight": case "DLeft": goDir = goRight; break;
	case "DOut":   case "DIn":   goDir = goIn; break;
	}
	for (var i = kids.length; i-- ;) {
	    update(kids[i],currs[i],nexts[i]);
	    goDir(kids[i]);
	}
	break;
    case "EContainer":
	update(node.childNodes[0],currE[2],nextE[2]);
	setPos(nextE[1],node.childNodes[0]);
	break;
    case "EEmpty":
	break;
    case "EHtml":
	if (next[1] !== curr[1]) {
	    var e = render(next);
	    node.parentNode.replaceChild(e,node);
	    node = e;
	}
	if (e.type !== 'button') {
	    var p = Value.getExcess(node);
	    next[3] -= p[0];
	    next[4] -= p[1];
	}
	break;
    case "EExternalHtml":
	if (next[1] !== curr[1])
	    node.parentNode.replaceChild(render(next),node);
	break;
    }
    if (next[3] !== curr[3]) node.style.width   = (~~next[3]) + 'px';
    if (next[4] !== curr[4]) node.style.height  = (~~next[4]) + 'px';
    if (next[5] !== curr[5]) node.style.opacity = next[5];
    if (next[6].length === 2) {
	var clr = Elm.Graphics.Color.extract(next[6][1]);
	if (clr !== node.style.backgroundColor) node.style.backgroundColor = clr;
    }
    if (next[7].length === 2) {
	if (curr[7].length === 1 || next[7][1] !== curr[7][1]) node.parentNode.href = next[7][1];
    }
    next[1] = curr[1];
}

return {render:render,update:update,addTo:addTo,newElement:newElement,flowWith:flowWith,goIn:goIn};

}(); 