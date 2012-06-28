document.addEventListener('redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
    });