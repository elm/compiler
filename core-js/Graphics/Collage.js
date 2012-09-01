
var Collage = function() {

function tracePoints(ctx,points) {
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i][1], points[i][2]);
    while (i--) { ctx.lineTo(points[i][1], points[i][2]); }
}

function solid(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.strokeStyle = ElmCode.Graphics.Color.extract(color);
    ctx.stroke();
};

function filled(ctx,color,points) {
    tracePoints(ctx,points);
    ctx.fillStyle = ElmCode.Graphics.Color.extract(color);
    ctx.fill();
}

function textured(redo,ctx,src,points) {
    var img = new Image();
    img.src = Foreign.JavaScript.castStringToJSString(src);
    img.onload = redo;
 
    tracePoints(ctx,points);
    ctx.fillStyle = ctx.createPattern(img,'repeat');
    ctx.fill();
}

function customLine(pattern,ctx,color,points) {
    if (pattern.length === 0) { pattern = [8,4]; }
    customLineHelp(ctx, pattern, points);
    ctx.strokeStyle = ElmCode.Graphics.Color.extract(color);
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
    var points = Foreign.JavaScript.castListToJSArray(form[3][1]);
    switch(form[1][0]) {
    case "Solid" : return solid(ctx,form[2],points);
    case "Dotted": return customLine([3,3],ctx,form[2],points);
    case "Dashed": return customLine([8,4],ctx,form[2],points);
    case "Custom": 
	var pattern = Foreign.JavaScript.castListToJSArray(form[1][1]);
	customLine(pattern,ctx,form[2],points);
    }
};

function drawShape(redo,ctx,shapeStyle,color,points) {
    points = Foreign.JavaScript.castListToJSArray(points);
    if (points.length > 0) points.push(points[0]);
    switch(shapeStyle[0]) {
    case "Filled":   return filled(ctx,color,points);
    case "Outlined": return solid(ctx,color,points);
    case "Textured": return textured(redo,ctx,shapeStyle[1],points);
    case "CustomOutline":
	var pattern = Foreign.JavaScript.castListToJSArray(shapeStyle[1]);
	customLine(pattern,ctx,color,points);
    }
};

function drawImage(redo,ctx,w,h,src) {
    var img = new Image();
    img.onload = redo;
    img.src = Foreign.JavaScript.castStringToJSString(src);
    ctx.drawImage(img,-w/2,-h/2,w,h);
}

function renderForm(redo,ctx,theta,scale,x,y,form) {
    ctx.save();
    if (x !== 0 || y !== 0) ctx.translate(x,y);
    if (theta !== ~~theta)  ctx.rotate(2*Math.PI*theta);
    if (scale !== 1)        ctx.scale(scale);
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
    canvas.style.width  = (~~w) + 'px';
    canvas.style.height = (~~h) + 'px';
    canvas.width  = ~~w;
    canvas.height = ~~h;
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	var w = canvas.width, h = canvas.height;
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

function updateCollage(node,currs,nexts) {
    if (currs.length !== nexts.length) {
	return node.parentNode.replaceChild(render(next),node); }
    if (nexts.length === 1) {
	return updateFormSet(node,currs[0],nexts[0]);
    }
    var kids = node.childNodes;
    var len = kids.length;
    for (var i = len; i--; ) {
	updateFormSet(kids[len-i-1], currs[i], nexts[i]);
    }
}

return {collage:collage, updateCollage:updateCollage};

}();