
var Collage = function() {

var JS = Elm.JavaScript;

function tracePoints(ctx,points) {
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i][1], points[i][2]);
    while (i--) { ctx.lineTo(points[i][1], points[i][2]); }
}

function solid(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.strokeStyle = Elm.Color.extract(color);
    ctx.stroke();
};

function filled(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.fillStyle = Elm.Color.extract(color);
    ctx.fill();
}

function textured(redo,ctx,src,points) {
    var img = new Image();
    img.src = JS.castStringToJSString(src);
    img.onload = redo;
 
    tracePoints(ctx,points);
    ctx.fillStyle = ctx.createPattern(img,'repeat');
    ctx.fill();
}

function customLine(pattern,ctx,color,points) {
    if (pattern.length === 0) { pattern = [8,4]; }
    customLineHelp(ctx, pattern, points);
    ctx.strokeStyle = Elm.Color.extract(color);
    ctx.stroke();
};

var customLineHelp = function(ctx, pattern, points) {
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i][1], y0 = points[i][2];
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
	x1 = points[i][1]; y1 = points[i][2];
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

function drawLine(ctx,form) {
    var points = form[3][1];
    switch(form[1][0]) {
    case "Solid" : return solid(ctx,form[2],points);
    case "Dotted": return customLine([3,3],ctx,form[2],points);
    case "Dashed": return customLine([8,4],ctx,form[2],points);
    case "Custom": return customLine(form[1][1],ctx,form[2],points);
    }
};

function drawShape(redo,ctx,shapeStyle,color,points) {
    switch(shapeStyle[0]) {
    case "Filled":   return filled(ctx,color,points);
    case "Outlined": return solid(ctx,color,points);
    case "Textured": return textured(redo,ctx,shapeStyle[1],points);
    case "CustomOutline":
	return customLine(shapeStyle[1],ctx,color,points);
    }
};

function drawImage(redo,ctx,w,h,src) {
    var img = new Image();
    img.onload = redo;
    img.src = JS.castStringToJSString(src);
    ctx.drawImage(img,-w/2,-h/2,w,h);
}

function renderForm(redo,ctx,theta,scale,x,y,form) {
    ctx.save();
    if (x !== 0 || y !== 0) ctx.translate(x,y);
    if (theta !== ~~theta)  ctx.rotate(2*Math.PI*theta);
    if (scale !== 1)        ctx.scale(scale,scale);
    ctx.beginPath();
    switch(form[0]) {
    case "FLine":  drawLine(ctx,form); break;
    case "FShape": drawShape(redo,ctx,form[1],form[2],form[3][1]); break;
    case "FImage": drawImage(redo,ctx,form[1],form[2],form[3]); break;
    }
    ctx.restore();
};

function renderForms(redo,ctx,w,h,forms) {
    ctx.clearRect(0,0,w,h);
    for (var i = forms.length; i--; ) {
	var f = forms[i];
	renderForm(redo,ctx,f[1],f[2],f[3][1],f[3][2],f[4]);
    }
}

function collageForms(w,h,forms) {
    var canvas = Render.newElement('canvas');
    w = ~~w;
    h = ~~h;
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.width  = w;
    canvas.height = h;
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	function redo() { renderForms(this,ctx,w,h,forms); }
	renderForms(redo,ctx,w,h,forms);
	return canvas;
    }
    canvas.innerHTML = "Your browser does not support the canvas element.";
    return canvas;
};

function collageElement(w,h,theta,scale,x,y,elem) {
    var e = Render.render(elem);
    var t = "translate(" + (x - elem[3] / 2) + "px,"+ (y - elem[4] / 2) + "px)";
    var r = theta === (~~theta) ? "" : "rotate(" + theta*360 + "deg)";
    var s = scale === 1 ? "" : "scale(" + scale + "," + scale + ")";
    var transforms = t + " " + s + " " + r;
    e.style.transform       = transforms;
    e.style.msTransform     = transforms;
    e.style.MozTransform    = transforms;
    e.style.webkitTransform = transforms;
    e.style.OTransform      = transforms;
    var div = Render.newElement('div');
    Render.addTo(div,e);
    div.style.width = (~~w) + "px";
    div.style.height = (~~h) + "px";
    div.style.overflow = "hidden";
    return div;
}

function collage(w,h,formss) {
    if (formss.length === 0) { return collageForms(w,h,[]); }
    var elems = new Array(formss.length);
    for (var i = formss.length; i--; ) {
	var f = formss[i];
	if (typeof f[0] === "string") {
	    elems[i] = collageElement(w,h,f[1],f[2],f[3][1],f[3][2],f[4][1]);
	} else {
	    elems[i] = collageForms(w,h,f);
	}
    }
    if (formss.length === 1) { return elems[0]; }
    return Render.flowWith(Render.goIn,function(x){return x},elems);
}

function updateFormSet(node,currSet,nextSet) {
    if (Value.eq(nextSet,currSet)) return;
    var w = node.style.width.slice(0,-2) - 0;
    var h = node.style.height.slice(0,-2) - 0;
    if (typeof nextSet[0] === "object") {
	if (typeof currSet[0] === "object") {
	    if (node.getContext) {
		var ctx = node.getContext('2d');
		function redo() { renderForms(this,ctx,w,h,nextSet); }
		return renderForms(redo,ctx,w,h,nextSet);
	    }
	}
	var newNode = collageForms(w,h,nextSet);
	newNode.style.position = 'absolute';
	return node.parentNode.replaceChild(newNode,node);
    }
    var f = nextSet;
    var newNode = collageElement(w,h,f[1],f[2],f[3][1],f[3][2],f[4][1]);
    newNode.style.position = 'absolute';
    return node.parentNode.replaceChild(newNode,node);
}

// assumes that the form sets are the same length.
function updateCollage(node,currs,nexts) {
    if (nexts.length === 1) {
	return updateFormSet(node,currs[0],nexts[0]);
    }
    var kids = node.childNodes;
    var len = kids.length;
    for (var i = len; i--; ) {
	updateFormSet(kids[len-i-1], currs[i], nexts[i]);
    }
}

function style(clr,n,list) {
    return ["Tuple2",
	    '<span style="font-size:100%;color:' + clr + ';">' + n + '</span>',
	    list];
}

function insideForm(point) { return function(form) {
    if (!inBoundsOf(point[1],point[2],form)) return false;
    var hw, hh;
    switch (form[4][0]) {
    case "FShape": return insideShape(point,form[1],form[2],form[3],form[4][3][1]);
    case "FLine":  return false;
    case "FImage":
	hw = form[4][1] / 2;
	hh = form[4][2] / 2;
	break;
    case "FElement":
	hw = form[4][1][3] / 2;
	hh = form[4][1][4] / 2;
	break;
    }
    return insideShape(point,form[1],form[2],form[3],
		       [ [null, hw, hh],
			 [null,-hw, hh],
			 [null,-hw,-hh],
			 [null, hw,-hh],
			 [null, hw, hh] ]);
    };
}

function inBoundsOf(px,py,form) {
  if (form.length < 6) {
    var fx = form[3][1], fy = form[3][2];
    var radiusSquared = 0;
    var scale = form[2];
    switch (form[4][0]) {
    case "FShape":
      var points = form[4][3][1];
      for (var i = points.length; --i; ) {
	  var p = points[i];
	  radiusSquared = Math.max(radiusSquared, p[1]*p[1] + p[2]*p[2]);
      }
      radiusSquared *= scale * scale;
      break;
    case "FLine":
      break;
    case "FImage":
      var x = scale * form[4][1] / 2;
      var y = scale * form[4][2] / 2;
      radiusSquared = x*x + y*y;
      break;
    case "FElement":
      var x = scale * form[4][1][3] / 2;
      var y = scale * form[4][1][4] / 2;
      radiusSquared = x*x + y*y;
      break;
    }
    form.push(function(px,py) {
	    var dx = px - fx;
	    var dy = py - fy;
	    return dx*dx + dy*dy < radiusSquared + 1;
	});
  }
  return form[5](px,py);
}

function insideShape(point,theta,scale,pos,points) {
  var counter = 0;
  var list = ["Nil"];
  var p1,p2;

  var x = (point[1] - pos[1]) / scale;
  var y = (point[2] - pos[2]) / scale;
  if (theta !== 0) {
      var t = -2 * Math.PI * theta;
      var nx = x * Math.cos(t) - y * Math.sin(t);
      y = x * Math.sin(t) + y * Math.cos(t);
      x = nx;
  }

  if (points.length === 0) { return false; }
  p1 = points[0];
  for (var i = points.length - 1; i--; ) {
    p2 = points[i];
    var p1x = p1[1], p1y = p1[2], p2x = p2[1], p2y = p2[2];

    if (p1y < p2y) {var ymin=p1y, ymax=p2y;} else {var ymin=p2y, ymax=p1y;}
    if (p1x < p2x) {var xmin=p1x, xmax=p2x;} else {var xmin=p2x, xmax=p1x;}

    if (ymin < y && y <= ymax && x <= xmax) {
	if (x <= xmin || x <= ((y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x)) {
	    ++counter;
	}
    }
    p1 = p2;
  }
  return (counter % 2) === 1;
}

return {collage:collage, updateCollage:updateCollage, insideForm:insideForm};

}();