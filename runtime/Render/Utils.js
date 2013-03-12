
ElmRuntime.Render.Utils = function() {
  'use strict';

  function htmlSize(txt, width) {
    var t = document.createElement('div');
    t.innerHTML = html;
    t.style.width  = w + "px";
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";
    
    document.body.appendChild(t);
    var cStyle = window.getComputedStyle(t,null);
    var realW = cStyle.getPropertyValue("width").slice(0,-2) - 0;
    var realH = cStyle.getPropertyValue("height").slice(0,-2) - 0;
    document.body.removeChild(t);
    //delete t;
    return { width: realW|0, height: realH|0 };
  }

  // This should not be useful, right?
  function nodeSize(e) {
    var t = e.cloneNode(true);
    
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";
    
    document.body.appendChild(t);
    var w = t.offsetWidth;
    var h = t.offsetHeight;
    document.body.removeChild(t);
    //delete t;
    return [w,h];
  }

  return { htmlSize: htmlSize };
}();