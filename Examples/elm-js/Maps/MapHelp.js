
var lib = document.createElement('script');

lib.addEventListener('load', function() {
	var div = document.createElement('div');
	div.id = "demoMap";
	div.style.width = "640px";
	div.style.height = "360px";
	
	var e = document.createEvent('Event');
	e.initEvent('provideMap', true, true);
	e.value = div;
	document.dispatchEvent(e);
	
	setTimeout(function() {
		var map = new OpenLayers.Map("demoMap");
		map.addLayer(new OpenLayers.Layer.OSM());
		map.zoomToMaxExtent();
	    }, 0);
    });

lib.src = "http://www.openlayers.org/api/OpenLayers.js";
document.head.appendChild(lib);