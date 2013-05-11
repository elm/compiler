
ElmRuntime.Render.Collage = function() {
'use strict';

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Matrix = Elm.Matrix2D({});
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

function renderForm(redo, ctx, form) {
    ctx.save();
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) ctx.translate(x, -y);
    if (theta !== 0) ctx.rotate(theta);
    if (scale !== 1) ctx.scale(scale, scale);
    ctx.beginPath();
    var f = form.form;
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FShape':
	if (f._0.ctor === 'Left') {
	    f._1.closed = true;
	    drawLine(ctx, f._0._0, f._1);
	} else {
            drawShape(redo, ctx, f._0._0, f._1);
        }
	break;
    }
    ctx.restore();
}

function makeTransform(form, matrix) {
    var m = matrix;
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) m = A3( Matrix.move, x, y, m );
    if (theta !== 0) m = A2( Matrix.rotate, theta, m );
    if (scale !== 1) m = Matrix.scale( scale );
    return m;
}

function collageElement(width, height, matrices, form) {
  var elem = form.form._0,
      e = Render.render(elem),
      m = A6( Matrix.matrix, 1, 0, 0, 1,
              (width - elem.props.width)/2,
              (elem.props.height - height)/2 );
  var len = matrices.length;
  for (var i = 0; i < len; ++i) { m = A2( Matrix.multiply, m, matrices[i] ); }
  m = makeTransform(form, m);
  var mtrx = 'matrix(' + m[0] + ',' + m[3] + ',' + m[1] + ',' +
                         m[4] + ',' + m[2] + ',' + (-m[5]) + ')';
  addTransform(e.style, mtrx);
  var div = newElement('div');
  addTo(div,e);
  div.style.width = width + 'px';
  div.style.height = height + 'px';
  div.style.overflow = 'hidden';
  div.style.position = 'absolute';
  return div;
}

function stepperHelp(list) {
    var arr = fromList(list);
    var i = 0;
    function hasNext() { return i < arr.length; }
    // assumes that there is a next element
    function next() {
        var out = arr[i];
        ++i;
        return out;
    }
    return { hasNext:hasNext, next:next };
}

function stepper(forms) {
    var ps = [stepperHelp(forms)];
    var matrices = [];
    function hasNext() {
        var len = ps.length;
        for (var i = 0; i < len; ++i ) {
            if (ps[i].hasNext()) return true;
        }
        return false;
    }
    // assumes that there is a next element
    function next(ctx) {
        while (!ps[0].hasNext()) { ps.shift(); matrices.pop(); ctx.restore(); }
        var out = ps[0].next();
        var f = out.form;
        if (f.ctor === 'FGroup') {
            ps.unshift(stepperHelp(f._1));
            var m = f._0;
            matrices.push(m);
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], -m[5]);
        }
        return out;
    }
    function transforms() { return matrices; }
    return { hasNext:hasNext, next:next, transforms:transforms };
}

function makeCanvas(w,h) {
    var canvas = newElement('canvas');
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.style.position = "absolute";
    canvas.width  = w;
    canvas.height = h;
    return canvas;
}

function render(model) {
    var w = model.w,
        h = model.h,
        div = newElement('div'),
        canvas = makeCanvas(w,h),
        ctx = canvas.getContext('2d');
    div.appendChild(canvas);
    ctx.translate(w/2, h/2);

    var stpr = stepper(model.forms);
    while (stpr.hasNext()) {
        var form = stpr.next(ctx);
        if (form.form.ctor === 'FElement') {
            div.appendChild(collageElement(w, h, stpr.transforms(), form));
            if (!stpr.hasNext()) return div;
            canvas = makeCanvas(w,h);
            div.appendChild(canvas);
            ctx = canvas.getContext('2d');
            ctx.translate(w/2, h/2);
            var transforms = stpr.transforms();
            var len = transforms.length;
            for (var i = 0; i < len; ++i) {
                var m = transforms[i];
                ctx.save();
                ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
            }
        } else if (form.form.ctor !== 'FGroup') {
            renderForm(function() { update(div, model, model); }, ctx, form);
        }
    }
    return div;
}

function update(node, oldModel, newModel) {
    if (!node.getContext) {
        return node.parentNode.replaceChild(render(newModel), node);
    }
    var ctx = node.getContext('2d');
    var w = newModel.w, h = newModel.h;
    ctx.clearRect(-w/2, -h/2, w, h);
}

return { render:render, update:update };

};