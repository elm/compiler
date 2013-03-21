
ElmRuntime.Render.Collage = function() {
'use strict';

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement, addTo = Utils.addTo,
    extract = Utils.extract, fromList = Utils.fromList,
    fromString = Utils.fromString, addTransform = Utils.addTransform;

function trace(ctx, path) {
    var points = fromList(path);
    console.log(points);
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i]._0, points[i]._1);
    while (i--) { ctx.lineTo(points[i]._0, points[i]._1); }
}

function line(ctx,style,path) {
    var isSolid = style.dashing.ctor === 'Nil';
    isSolid ? trace(ctx, path) : customLineHelp(ctx, style, path);
    ctx.stroke();
}

function customLineHelp(ctx, pattern, points) {
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
}

function drawLine(ctx, style, path) {
    ctx.lineWidth = style.width;
    ctx.lineCap = style.cap.ctor.toLowerCase();
    ctx.lineJoin = style.join.ctor.toLowerCase();
    ctx.miterLimit = style.miterLimit;
    ctx.strokeStyle = extract(style.color);
    return line(ctx, style, path);
}

function texture(redo, ctx, src) {
    var img = new Image();
    img.src = fromString(src);
    img.onload = redo;
    return ctx.createPattern(img, 'repeat');
}

function gradient(ctx, grad) {
    var g;
    if (grad.ctor === 'Linear') {
	var p1 = grad._1, p2 = grad._2;
	g = ctx.createLinearGradient(p1._0, p1._1, p2._0, p2._1);
    } else {
	var p1 = grad._1, p2 = grad._3;
	g = ctx.createRadialGradient(p1._0, p1._1, grad._2, p2._0, p2._1, grad._4);
    }
    var stops = fromList(grad._0);
    for (var i = stops.length; i--; ) {
	var stop = stops[i];
	g.addColorStop(stop._0, extract(stop._1));
    }
    return g;
}

function drawShape(redo, ctx, style, path) {
    console.log(style, path);
    trace(ctx, path);
    var sty = style.ctor;
    ctx.fillStyle =
        sty === 'Solid' ? extract(style._0) :
        sty === 'Texture' ? texture(redo, ctx, style._0) : gradient(ctx, style._0);
    ctx.fill();
}

function drawImage(redo, ctx, form) {
    var img = new Image();
    img.onload = redo;
    img.src = fromString(form._3);
    var w = form._0, h = form._1, pos = form._2;
    console.log(w,h,pos);
    ctx.drawImage(img, pos._0, pos._1, w, h, -w/2, -h/2, w, h);
}

function renderForm(redo,ctx,form) {
    ctx.save();
    if (form.x !== 0 || form.y !== 0) ctx.translate(form.x, form.y);
    if (form.theta !== 0) ctx.rotate(form.theta);
    if (form.scale !== 1) ctx.scale(form.scale, form.scale);
    ctx.beginPath();
    var f = form.form;
    console.log(f);
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FShape':
	if (form.form._0.ctor === 'Left') drawLine(ctx, f._0._0, f._1);
	else drawShape(redo, ctx, f._0._0, f._1);
	break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FGroup': renderForms(redo, ctx, f._0); break;
    }
    ctx.restore();
}

function renderForms(redo, ctx, forms) {
    var fs = fromList(forms);
    console.log(fs);
    for (var i = 0, len = fs.length; i < len; ++i) {
	renderForm(redo,ctx,fs[i]);
    }
}

function collageForms(w,h,forms) {
    var canvas = newElement('canvas');
    w = w|0;
    h = h|0;
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.width  = w;
    canvas.height = h;
    function redo() { renderForms(this,ctx,forms); }
    if (canvas.getContext) {
	var ctx = canvas.getContext('2d');
	console.log(forms);
	renderForms(redo,ctx,forms);
	return canvas;
    }
    canvas.innerHTML = "Your browser does not support canvas.";
    return canvas;
}

function collageElement(w, h, m, elem) {
  var e = Render.render(elem);
  var matrix = 'matrix(' + m[0] + ',' + m[3] + ',' + m[1] + ',' +
                           m[4] + ',' + m[2] + ',' + m[5] + ')';
  addTransform(e.style, matrix);
  var div = newElement('div');
  addTo(div,e);
  div.style.width = (w|0) + "px";
  div.style.height = (h|0) + "px";
  div.style.overflow = "hidden";
  return div;
}

function render(model) {
    console.log(model);
    return collageForms(model.w, model.h, model.forms);
}

return { render:render };

};