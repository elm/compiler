
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

function init(display, container, module) {
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

  var Module = module(elm);
  inputs = ElmRuntime.filterDeadInputs(inputs);

  // If graphics are not enabled, escape early, skip over setting up DOM stuff.
  if (display !== ElmRuntime.Display.NONE) { initGraphics(elm, Module); }

  return { send:send, recv:recv,
          __private__ : { inputs:inputs, container:container } };
};

function initGraphics(elm, Module) {
  if (!('main' in Module)) throw new Error("'main' is missing! What do I display?!");
  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  if (!('recv' in signalGraph)) {
      var Signal = Elm.Signal(elm);
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;
  
  // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  container.appendChild(Render.render(currentScene));
  if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
  
  // set up updates so that the DOM is adjusted as necessary.
  function domUpdate(newScene) {
      ElmRuntime.draw(function(_) {
              Render.update(container.firstChild, currentScene, newScene);
              currentScene = newScene;
              if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
          });
      return newScene;
  }
  A2(Signal.lift, domUpdate, signalGraph);
}

}());