
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