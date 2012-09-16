
var eq = Value.eq;

Signal.addListener(document, 'elm_log', function(e) { console.log(e.value); });
Signal.addListener(document, 'elm_title', function(e) { document.title = e.value; });
Signal.addListener(document, 'elm_redirect', function(e) {
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
  var includeAs = function(name) { return function(library) {
	includeGlobal[name] = includeGlobal[name] || {};
	for (var i in library) {
	    if (i === 'Internal') continue;
	    includeGlobal[name][i] = library[i];
	}
    };
  };
  include (Element);
  include (Text);

  color = Element.color;
  height = Element.height;
  show = Value.show;
  
  include (ElmCode.Graphics.Color);
  include (ElmCode.Graphics.Element);

  includeAs ('Time')     (Signal.Time);
  includeAs ('Mouse')    (Signal.Mouse);
  includeAs ('Keyboard') (Signal.Keyboard);
  includeAs ('Window')   (Signal.Window);
  includeAs ('HTTP')     (Signal.HTTP);
  includeAs ('Input')    (Signal.Input);
  includeAs ('Random')   (Signal.Random);

}());

var ElmCode = ElmCode || {};
ElmCode.Data = Data;
ElmCode.Signal = Signal;
ElmCode.Data.List = List;
ElmCode.Foreign = Foreign;
ElmCode.Prelude = Prelude;
ElmCode.Graphics.Text = Text;