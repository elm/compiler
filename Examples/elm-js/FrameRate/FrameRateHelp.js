
var FrameRate = function() {

var desiredFPS = 40;
var msPerFrame = 1000 / desiredFPS;

// Set the desired rate of frames per second.
// Default is 40 if no desiredFPS event is triggered.
document.addEventListener('desiredFPS', function(e) {
	console.log(e.value);
	desiredFPS = e.value;
	msPerFrame = 1000 / desiredFPS;
    });


var date = new Date();
var startTime = date.getTime();
var frameStart = startTime;
var frameEnd   = startTime;
var timeoutID = 0;

// Trigger time events to try to maintain the desired FPS.
document.addEventListener('finished', function(evt) {
	clearTimeout(timeoutID);
	date = new Date();
	frameEnd = date.getTime();
	var diff = frameEnd - frameStart;
	var waitTime = Math.max(msPerFrame-diff,3);
	timeoutID = setTimeout(function() {
		date = new Date();
		frameStart = date.getTime();
		e = document.createEvent('Event');
		e.initEvent('trigger', true, true);
		e.value = (frameStart - startTime) / 1000;
		document.dispatchEvent(e);
	    }, waitTime);
    });

}();