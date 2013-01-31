/*! Window !*/

Elm.Window = function() {

  /*[Dimensions]*/

  /** dimensions :: Signal (Int,Int)
      The current dimensions of the window (i.e. the area viewable to the
      user, not including scroll bars).
  **/
  var dimensions = Elm.Signal.constant(Value.Tuple(window.innerWidth,
						   window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  /** width :: Signal Int
      The current width of the window.
  **/
  var width  = Elm.Signal.lift(function(p){return p[1];})(dimensions);
  width.defaultNumberOfKids = 0;

  /** height :: Signal Int
      The current height of the window.
  **/
  var height = Elm.Signal.lift(function(p){return p[2];})(dimensions);
  height.defaultNumberOfKids = 0;

  Value.addListener(window, 'resize', function(e) {
	  var w = document.getElementById('widthChecker').offsetWidth;
	  var hasListener = Dispatcher.notify(dimensions.id,
					      Value.Tuple(w, window.innerHeight));
	  if (!hasListener)
		this.removeEventListener('resize',arguments.callee,false);
	});
  return {dimensions:dimensions,width:width,height:height};
}();
