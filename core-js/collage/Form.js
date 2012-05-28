
var Form = function() {
  var move = function(x) { return function(y) { return function(form) {
        var t = form[1];
        return [form[0], [[x + t[0][0], y + t[0][1]], t[1], t[2]]];
      };
    };
  };
  var rotate = function(theta) { return function(form) {
      var t = form[1];
      return [form[0], [t[0], t[1] + 2* Math.PI*theta, t[2]]];
    };
  };
  var scale = function(s) { return function(form) {
      var t = form[1];
      return [form[0], [t[0], t[1], t[2] * s]];
    };
  };
  var create = function(howToDraw, center) {
    return [howToDraw, Transforms.create(center, 0, 1)];
  };
  var draw = function(form) {
      ctx.save();
      var trans = form[1];
      ctx.translate(trans[0][0], trans[0][1]);
      ctx.rotate(trans[1]);
      ctx.scale(trans[2], trans[2]);
      form[0](ctx);
      ctx.restore();
      return ctx;
  };
  return {create:create, move:move, rotate:rotate, scale:scale, draw:draw};
}();