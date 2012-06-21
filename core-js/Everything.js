
var eq = function(x,y) {
    if (typeof x === "object") {
	if (x === y) return true;
	if (x.length !== y.length) return false;
	for (var i = x.length; i--; ) {
	    if (!eq(x[i],y[i])) return false;
	}
	return true;
    }
    return x === y;
};

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
  
  include (Color);
  include (Shape);
  include (Line);

  includeAs ('Time')     (Signal.Time);
  includeAs ('Mouse')    (Signal.Mouse);
  includeAs ('Keyboard') (Signal.Keyboard);
  includeAs ('Window')   (Signal.Window);
  includeAs ('HTTP')     (Signal.HTTP);
  includeAs ('Input')    (Signal.Input);
  includeAs ('Random')   (Signal.Random);

}());

var ElmCode = {};
ElmCode.Data = Data;
ElmCode.Signal = Signal;
ElmCode.Data.List = List;
ElmCode.Prelude = Prelude;