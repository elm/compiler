
ElmRuntime.Render.Collage = function() {
'use strict';

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement, addTo = Utils.addTo,
    extract = Utils.extract, fromList = Utils.fromList,
    fromString = Utils.fromString, addTransform = Utils.addTransform;

function trace(ctx, path) {
    var points = fromList(path);
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i]._0, points[i]._1);
    while (i--) { ctx.lineTo(points[i]._0, points[i]._1); }
    if (path.closed) {
	i = points.length - 1;
	ctx.lineTo(points[i]._0, points[i]._1);
    }
}

function line(ctx,style,path) {
    style.dashing.ctor === 'Nil' ? trace(ctx, path) : customLineHelp(ctx, style, path);
    ctx.stroke();
}

function customLineHelp(ctx, style, path) {
    var points = fromList(path);
    if (path.closed) points.push(points[0]);
    var pattern = fromList(style.dashing);
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i]._0, y0 = points[i]._1;
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
	x1 = points[i]._0; y1 = points[i]._1;
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
    var join = style.join.ctor;
    ctx.lineJoin = join === 'Smooth' ? 'round' :
                   join === 'Sharp' ? 'miter' : 'bevel';
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
    ctx.drawImage(img, pos._0, pos._1, w, h, -w/2, -h/2, w, h);
}

function renderForm(stack, redo, ctx, form) {
    ctx.save();
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) ctx.translate(x, -y);
    if (theta !== 0) ctx.rotate(theta);
    if (scale !== 1) ctx.scale(scale, scale);
    ctx.beginPath();
    var f = form.form;
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FShape':
	if (f._0.ctor === 'Left') {
	    f._1.closed = true;
	    drawLine(ctx, f._0._0, f._1);
	} else {
            drawShape(redo, ctx, f._0._0, f._1);
        }
	break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FGroup':
	ctx.save();
	var m = f._0;
        // stack.push(makeTransform(x, y, theta, scale, m));
	ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
	renderForms(stack, redo, ctx, f._1);
        // stack.pop();
	ctx.restore();
	break;
    }
    ctx.restore();
}

function makeTransform(x, y, theta, scale, matrix) {
    return function(m) {
        if (x !== 0 || y !== 0) m = A3( Matrix.move, x, y, m );
        if (theta !== 0) m = A2( Matrix.rotate, theta, m );
        if (scale !== 1) m = Matrix.scale( scale );
        return Matrix.multiply( m, matrix );
    };
}

function renderForms(stack, redo, ctx, forms) {
    var fs = fromList(forms);
    for (var i = 0, len = fs.length; i < len; ++i) {
	renderForm(stack, redo, ctx, fs[i]);
    }
}

function collageForms(w,h,forms) {
    var canvas = newElement('canvas'),
        devicePixelRatio = window.devicePixelRatio || 1;

    if(window.webkitImageSmoothingEnabled) {
      window.webkitImageSmoothingEnabled = true;
    }

    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.width  = w;
    canvas.height = h;
    if (canvas.getContext) {
        var ctx = canvas.getContext('2d'),
            backingStoreRatio = ctx.webkitBackingStorePixelRatio ||
                ctx.mozBackingStorePixelRatio ||
                ctx.msBackingStorePixelRatio ||
                ctx.oBackingStorePixelRatio ||
                ctx.backingStorePixelRatio || 1,

            ratio = devicePixelRatio / backingStoreRatio;
        canvas.width  = ratio * w;
        canvas.height = ratio * h;
        ctx.scale(ratio, ratio);
        ctx.translate(w/2, h/2);
        var redo = function() { renderForms([], this, ctx, forms); };
        renderForms([], redo, ctx, forms);
        return canvas;
    }
    canvas.innerHTML = "Your browser does not support canvas.";
    return canvas;
}

function collageElement(stack, width, height, elem) {
  var e = Render.render(elem),
      w = elem.props.width,
      h = elem.props.height,
      m = Matrix.matrix(1,0,0,1,width/2,height/2),
      len = stack.length;
  for (var i = 0; i < len; ++i) { m = stack[i](m); }
  var mtrx = 'matrix(' + m[0] + ',' + m[3] + ',' + m[1] + ',' +
                         m[4] + ',' + m[2] + ',' + m[5] + ')';
  addTransform(e.style, 'translate(' + (-w/2) + 'px,'+ (-h/2) + 'px) ' + mtrx);
  var div = newElement('div');
  addTo(div,e);
  div.style.width = width + 'px';
  div.style.height = height + 'px';
  div.style.overflow = 'hidden';
  return div;
}

function render(model) {
    return collageForms(model.w, model.h, model.forms);
}

function update(node, oldModel, newModel) {
    if (!node.getContext) {
        return node.parentNode.replaceChild(render(newModel), node);
    }
    var ctx = node.getContext('2d');
    var w = newModel.w, h = newModel.h;
    ctx.clearRect(-w/2, -h/2, w, h);
    function redo() { renderForms( [], this, ctx, newModel.forms ); }
    renderForms( [], redo, ctx, newModel.forms );
}

return { render:render, update:update };

};
