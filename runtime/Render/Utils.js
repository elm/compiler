ElmRuntime.Render.Utils = function() {
  function newElement(elementType) {
      var e = document.createElement(elementType);
      e.style.padding = "0";
      e.style.margin = "0";
      return e;
  }

  function addTo(container, elem) {
      container.appendChild(elem);
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

  var List = Elm.Native.List.make({});

  return {addTo:addTo,
          newElement: newElement,
          colorToCss: Elm.Native.Color.make({}).toCss,
          fromList: List.toArray,
          fromString: function(s) { return s; },
          toString: function(s) { return s; },
          eq: Elm.Native.Utils.make({}).eq,
          addTransform: addTransform,
          removeTransform: removeTransform};
};
