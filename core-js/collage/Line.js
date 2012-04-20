
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
