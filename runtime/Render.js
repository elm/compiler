
var ElmRuntime = ElmRuntime || {};
ElmRuntime.Render = function() {
'use strict';

function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
}

function addTo(container, elem) {
    container.appendChild(elem);
}

function makeText(pos,txt) {
    var e = newElement('div');
    e.innerHTML = txt;
    e.style.textAlign = pos;
    return e;
}

function extract(c) {
    if (c._3 === 1) { return 'rgb(' + c._0 + ',' + c._1 + ',' + c._2 + ')'; }
    return 'rgba(' + c._0 + ',' + c._1 + ',' + c._2 + ',' + c._3 + ')';
}

function setProps(props, e) {
    e.style.width  = (props.width |0) + 'px';
    e.style.height = (props.height|0) + 'px';
    if (props.opacity !== 1) { e.style.opacity = props.opacity; }
    if (props.color.ctor === 'Just') {
	e.style.backgroundColor = extract(props.color._0);
    }
    if (props.tag !== '') { e.id = props.tag; }
    if (props.href !== '') {
	var a = newElement('a');
	a.href = props.href;
	addTo(a,e);
	return a;
    }
    return e;
}

function image(props, img) {
  switch (img._0.ctor) {
  case 'Plain':   return plainImage(img._3);
  case 'Fitted':  return fittedImage(props.width, props.height, img._3);
  case 'Cropped': return croppedImage(img._0._0,props.width,props.height,img._3);
  }
}

function plainImage(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function fittedImage(w, h, src) {
    var e = newElement('div');
    e.style.position = "relative";
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
	img.style.position = 'absolute';
	img.style.margin = 'auto';

	var sw = w, sh = h;
	if (w / h > this.width / this.height) {
	    sh = Math.round(this.height * w / this.width);
	} else {
	    sw = Math.round(this.width * h / this.height);
	}
	img.style.width = sw + 'px';
	img.style.height = sh + 'px';
	img.style.left = ((w - sw) / 2) + 'px';
	img.style.top = ((h - sh) / 2) + 'px';
    };
    img.src = src;
    img.name = src;
    addTo(e,img);
    return e;
}

function croppedImage(pos, w, h, src) {
    var e = newElement('div');
    e.style.position = "relative";
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
	img.style.position = 'absolute';
	img.style.margin = 'auto';
	var sw = dimensions.width / w, sh = dimensions.height / h;
	img.style.width = (this.width * sw) + 'px';
	img.style.height = (this.height * sh) + 'px';
	img.style.left = (- pos._0 * sw) + 'px';
	img.style.top = (- pos._1 * sh) + 'px';
    };
    img.src = src;
    img.name = src;
    addTo(e,img);
    return e;
}

function goIn(e) { e.style.position = 'absolute'; return e; }
function goDown(e) { return e }
function goRight(e) { e.style.styleFloat = e.style.cssFloat = "left"; return e; }
function flowWith(f, array) {
    var container = newElement('div');
    for (var i = array.length; i--; ) {
	addTo(container, f(render(array[i])));
    }
    return container;
}

function flow(dir,elist) {
    var array = JS.fromList(elist);
    switch(dir) {
    case "DDown":  array.reverse();
    case "DUp":    return flowWith(goDown,array);
    case "DRight": array.reverse();
    case "DLeft":  return flowWith(goRight,array);
    case "DOut":   array.reverse();
    case "DIn":    return flowWith(goIn,array);
    }
}

function toPos(pos) {
    switch(pos[0]) {
    case "Absolute": return  pos[1] + "px";
    case "Relative": return (pos[1] * 100) + "%";
    }
}

function addTransform(style, trans) {
  style.transform       = trans;
  style.msTransform     = trans;
  style.MozTransform    = trans;
  style.webkitTransform = trans;
  style.OTransform      = trans;
}

function removeTransform(style) {
  style.transform       = 'none';
  style.msTransform     = 'none';
  style.MozTransform    = 'none';
  style.webkitTransform = 'none';
  style.OTransform      = 'none';
}

function setPos(pos,w,h,e) {
  e.style.position = 'absolute';
  e.style.margin = 'auto';
  var transform = '';
  switch(pos.horizontal) {
  case 'P': e.style.right = toPos(pos.x); break;
  case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
  case 'N': e.style.left = toPos(pos.x); break;
  }
  switch(pos.vertical) {
  case 'N': e.style.bottom = toPos(pos.x); break;
  case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
  case 'P': e.style.top = toPos(pos.x); break;
  }
  if (transform !== '') addTransform(e.style, transform);
  return e;
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos, elem.props.width, elem.props.height, e);
    var div = newElement('div');
    div.style.position = 'relative';
    div.style.overflow = 'hidden';
    addTo(div,e);
    return div;
}

function rawHtml(html) {
    var e = newElement('div');
    e.innerHTML = html;
    return e;
}

function render(elem) { return setProps(elem.props, makeElement(elem)); }
function makeElement(e) {
    switch(e.element.ctor) {
    case 'Image':     return image(e.props, e.element);
    case 'Flow':      return flow(e.element._0, e.element._1);
    case 'Container': return container(e.element._0, e.element._1);
    case 'Spacer':    return newElement('div');
    case 'RawHtml':   return rawHtml(e.element._0);
    case 'DomNode':   return e.element._0;
    }
}

function update(node, curr, next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr.props.id === next.props.id) return false;
    if (curr.element.ctor !== next.element.ctor) {
	node.parentNode.replaceChild(render(next),node);
	return true;
    }
    var nextE = next.element, currE = curr.element;
    switch(nextE.ctor) {
    case "Spacer": break;
    case "RawHtml": if (nextE._0 !== currE._0) node.innerHTML = nextE._0; break;
    case "Image":
	if (nextE._0.ctor === 'Plain') {
	    if (nextE._3 !== currE._3) node.src = nextE._3;
	} else if (!Value.eq(nextE,currE) ||
		   next.props.width !== curr.props.width ||
		   next.props.height !== curr.props.height) {
	    node.parentNode.replaceChild(render(next),node);
	    return true;
	}
	break;
    case "Flow":
	if (nextE._0.ctor !== currE._0.ctor) {
	    node.parentNode.replaceChild(render(next),node);
	    return true;
	}
	var nexts = nextE._1;
	var kids = node.childNodes;
	if (nexts.length !== kids.length) {
	    node.parentNode.replaceChild(render(next),node);
	    return true;
	}
	var currs = currE._1;
	var goDir = function(x) { return x; };
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
    case "Container":
	var inner = node.firstChild;
	if (!update(inner, currE._1, nextE._1)) {
	    if (nextE._0.horizontal.ctor !== currE._0.horizontal.ctor) {
		inner.style.left = inner.style.right = 'none';
		removeTransform(inner.style);
	    }
	    if (nextE._0.vertical.ctor !== currE._0.vertical.ctor) {
		inner.style.top = inner.style.bottom = 'none';
		removeTransform(inner.style);
	    }
	}
	setPos(nextE._0, next.props.width, next.props.height, node.firstChild);
	break;
    case "DomNode":
	if (next._0 !== curr._0) node.parentNode.replaceChild(render(next),node);
	break;
    }
    var props = next.props, currP = curr.props;
    if (props.width !== currP.width)   e.style.width  = (props.width |0) + 'px';
    if (props.height !== currP.height) e.style.height = (props.height|0) + 'px';
    if (props.opacity !== 1 && props.opacity !== currP.opacity) {
	e.style.opacity = props.opacity;
    }
    var nextColor = (props.color.ctor === 'Just' ?
		     extract(props.color._0) : 'transparent');
    if (e.style.backgroundColor !== nextColor) e.style.backgroundColor = nextColor;
    if (props.tag !== currP.tag) { e.id = props.tag; }
    if (props.href !== currP.href) {
	if (currP.href === '') {
	    var a = newElement('a');
	    a.href = props.href;
	    addTo(a,e);
	    e.parentNode.replaceChild(a,e);
	} else {
	    node.parentNode.href = props.href;
	}
    }
}

return {
    render:render,
    update:update,
    addTo:addTo,
    newElement:newElement
};

}();