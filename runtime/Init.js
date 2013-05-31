
(function() {
'use strict';

Elm.fullscreen = function(module) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module);
};

Elm.byId = function(id, module) {
    var container = document.getElementById(id);
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.byId must be given a div, not a ' + tag + '.');
    }
    while (container.hasChildNodes()) {
        container.removeChild(container.lastChild);
    }
    return init(ElmRuntime.Display.COMPONENT, container, module);
};

Elm.worker = function(module) {
    return init(ElmRuntime.Display.NONE, {}, module);
};

function init(display, container, module, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

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

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = { notify:notify,
              node:container,
              display:display,
              id:ElmRuntime.guid(),
              inputs:inputs
  };

  // Set up methods to communicate with Elm program from JS.
  function send(name, value) {
      if (typeof value === 'undefined') return function(v) { return send(name,v); };
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

  function swap(newModule) {
      var div = document.createElement('div');
      if (container.id) { div.id = container.id; }
      var newElm = init(display, div, newModule, elm);
      // elm.send = newElm.send;
      // elm.recv = newElm.recv;
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = module(elm);
  inputs = ElmRuntime.filterDeadInputs(inputs);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.value = A2( Elm.Graphics.Element(elm).spacer, 0, 0 );
          graphicsNode.recv(0, true, 0);
      }
  }

  return { send:send, recv:recv, swap:swap };
};

function initGraphics(elm, Module) {
  if (!('main' in Module))
      throw new Error("'main' is missing! What do I display?!");

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;
  
  // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));
  if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
  
  // set up updates so that the DOM is adjusted as necessary.
  function domUpdate(newScene, currentScene) {
      ElmRuntime.draw(function(_) {
              Render.update(elm.node.firstChild, currentScene, newScene);
              if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
          });
      return newScene;
  }
  return A3(Signal.foldp, F2(domUpdate), currentScene, signalGraph);
}

}());