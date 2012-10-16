Elm.Window = function() {
  var dimensions = Elm.Signal.constant(Value.Tuple(window.innerWidth,
						   window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  var width  = Elm.Signal.lift(function(p){return p[1];})(dimensions);
  width.defaultNumberOfKids = 0;
  var height = Elm.Signal.lift(function(p){return p[2];})(dimensions);
  height.defaultNumberOfKids = 0;

  addListener(window, 'resize', function(e) {
	  var w = document.getElementById('widthChecker').offsetWidth;
	  var hasListener = Dispatcher.notify(dimensions.id,
					      Value.Tuple(w, window.innerHeight));
	  if (!hasListener)
		this.removeEventListener('resize',arguments.callee,false);
	});
  return {dimensions:dimensions,width:width,height:height};
}();
