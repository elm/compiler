
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
    if (c._3 === 1) { return 'rgb(' + c._0 + ',' + c._1 + ',' + c._2 + ')'; }
    return 'rgba(' + c._0 + ',' + c._1 + ',' + c._2 + ',' + c._3 + ')';
}

var fromList = Elm.JavaScript({}).fromList;
var eq = Elm.Native.Utils({}).eq;

return {addTo:addTo,
	newElement:newElement,
	extract:extract,
	fromList:fromList,
	eq:eq
	};
};
