
(function() {
'use strict';

Elm.fullscreen = function(module) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    return init(ElmRuntime.Display.FULLSCREEN, document.body, module);
};

Elm.node = function(width, height, module) {
    var container = document.createElement('div');
    container.style.width = width + 'px';
    container.style.height = height + 'px';
    return init(ElmRuntime.Display.COMPONENT, container, module);
};

Elm.worker = function(module) {
    return init(ElmRuntime.Display.NONE, {}, module);
};

function init(display, container, module) {
  // defining state needed for an instance of the Elm RTS
  var signalGraph = null;
  var inputs = [];
  var visualModel = null;
  
  function notify(id, v) {
    var timestep = Date.now();
    var hasListener = false;
    for (var i = inputs.length; i--; ) {
      // must maintain the order of this stmt to avoid having the ||
      // short-circuiting the necessary work of recv
      hasListener = inputs[i].recv(timestep, id, v) || hasListener;
    }
    return hasListener;
  }

  container.offsetX = 0;
  container.offsetY = 0;

  // create the actuall RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = { notify:notify,
              node:container,
              display:display,
              id:ElmRuntime.guid(),
              inputs:inputs
  };

  // Set up methods to communicate with Elm program from JS.
  function send(name, value) {
      if (typeof value === 'undefined') return function(v) { return send(name,v) };
      var e = document.createEvent('Event');
      e.initEvent(name + '_' + elm.id, true, true);
      e.value = value;
      document.dispatchEvent(e);
  }
  function recv(name, handler) {
      document.addEventListener(name + '_' + elm.id, handler);
  }

  recv('log', function(e) {console.log(e.value)});
  recv('title', function(e) {document.title = e.value});
  recv('redirect', function(e) {
	if (e.value.length > 0) { window.location = e.value; }
      });

  // If graphics are not enabled, escape early, skip over setting up DOM stuff.
  if (display === ElmRuntime.Display.NONE) {
      return { send : send, recv : recv };
  }

  var Render = ElmRuntime.use(ElmRuntime.Render.Element);

  // evaluate the given module and extract its 'main' value.
  signalGraph = module(elm).main;
  
  // make sure the signal graph is actually a signal, extract the visual model,
  // and filter out any unused inputs.
  var Signal = Elm.Signal(elm);
  if (!('recv' in signalGraph)) signalGraph = Signal.constant(signalGraph);
  visualModel = signalGraph.value;
  inputs = ElmRuntime.filterDeadInputs(inputs);
  
   // Add the visualModel to the DOM
  var renderNode = Render.render(visualModel)
  container.appendChild(renderNode);
  elm.Native.Window.resizeIfNeeded();
  
  // set up updates so that the DOM is adjusted as necessary.
  var update = Render.update;
  function domUpdate(value) {
      ElmRuntime.draw(function(_) {
              update(renderNode, visualModel, value);
              visualModel = value;
              elm.Native.Window.resizeIfNeeded();
          });
      return value;
  }
  
  signalGraph = A2(Signal.lift, domUpdate, signalGraph);
    
  return { send : send, recv : recv, node : container };
};

}());