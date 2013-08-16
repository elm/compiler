
ElmRuntime.Render.Utils = function() {
'use strict';

function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
}

function addTo(container, elem) {
    container.appendChild(elem);
}

function extract(c) {
    if (c._3 === 1) { return 'rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')'; }
    return 'rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')';
}

function addTransform(style, trans) {
  style.transform       = trans;
  style.msTransform     = trans;
  style.MozTransform    = trans;
  style.webkitTransform = trans;
  style.OTransform      = trans;
}

function removeTransform(style) {
  style.transform       = 'none';
  style.msTransform     = 'none';
  style.MozTransform    = 'none';
  style.webkitTransform = 'none';
  style.OTransform      = 'none';
}

var List = Elm.Native.List({});

return {addTo:addTo,
	newElement:newElement,
	extract : extract,
	fromList: List.toArray,
	fromString: function(s) { return List.toArray(s).join(''); },
	toString: List.fromArray,
	eq: Elm.Native.Utils({}).eq,
	addTransform: addTransform,
	removeTransform: removeTransform
	};
};
