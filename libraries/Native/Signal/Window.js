/*
import Signal
import Native.Misc
*/

(function() {
  'use strict';

  var Misc = Elm.Native.Misc;

  var dimensions = Elm.Signal.constant(Misc.Tuple(window.innerWidth,
						  window.innerHeight));
  dimensions.defaultNumberOfKids = 2;

  var width  = Elm.Signal.lift(function(p){return p._0;})(dimensions);
  width.defaultNumberOfKids = 0;

  var height = Elm.Signal.lift(function(p){return p._1;})(dimensions);
  height.defaultNumberOfKids = 0;

  Misc.addListener(window, 'resize', function(e) {
	  var w = document.getElementById('widthChecker').offsetWidth;
	  var hasListener = Dispatcher.notify(dimensions.id,
					      Misc.Tuple(w, window.innerHeight));
	  if (!hasListener)
		this.removeEventListener('resize',arguments.callee,false);
	});
  Elm.Native.Window = {dimensions:dimensions,width:width,height:height};
}());
