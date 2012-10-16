
Value.addListener(document, 'elm_log', function(e) { console.log(e.value); });
Value.addListener(document, 'elm_title', function(e) {document.title = e.value;});
Value.addListener(document, 'elm_redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
    });

var includeGlobal = this;
(function() {
  var include = function(library) {
    for (var i in library) {
	if (i === 'Internal') continue;
	try {
	    includeGlobal[i] = library[i];
	} catch (err) {
	    if (i === 'length') {
		includeGlobal.execScript('var length;');
		length = library[i];
		continue;
	    }
	}
    }
  };
  include (Graphics.Element);
  include (Graphics.Text);

  color = Element.color;
  height = Element.height;
  show = Value.show;
  
}());
