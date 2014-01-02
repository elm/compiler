
(function() {
'use strict';

Elm.fullscreen = function(module, ports) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module, ports);
};

Elm.embed = function(module, container, ports) {
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
    } else if (container.hasChildNodes()) {
        throw new Error('Elm.node must be given an empty DIV. No children allowed!');
    }
    return init(ElmRuntime.Display.COMPONENT, container, module, ports);
};

Elm.worker = function(module, ports) {
    return init(ElmRuntime.Display.NONE, {}, module, ports);
};

function init(display, container, module, moduleToReplace, ports) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  var updateInProgress = false;
  function notify(id, v) {
      if (updateInProgress) {
          throw new Error(
              'The notify function has been called synchronously!\n' +
              'This can lead to frames being dropped.\n' +
              'Definitely report this to <https://github.com/evancz/Elm/issues>\n');
      }
      updateInProgress = true;
      var timestep = Date.now();
      for (var i = inputs.length; i--; ) {
          inputs[i].recv(timestep, id, v);
      }
      updateInProgress = false;
  }

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {
      notify:notify,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs,
      ports_in:ports
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
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, elm);
      inputs = [];
      // elm.send = newElm.send;
      // elm.recv = newElm.recv;
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = {};
  var reportAnyErrors = function() {};
  try {
      Module = module.make(elm);
  } catch(e) {
      var directions = "<br/>&nbsp; &nbsp; Open the developer console for more details."
      Module.main = Elm.Text.make(elm).text('<code>' + e.message + directions + '</code>');
      reportAnyErrors = function() { throw e; }
  }
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.recv(0, true, 0);
      }
  }

  reportAnyErrors();
  return { send:send, recv:recv, swap:swap, ports:elm.ports_out };
};

function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function initGraphics(elm, Module) {
  if (!('main' in Module))
      throw new Error("'main' is missing! What do I display?!");

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal.make(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;

 // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));

  // set up updates so that the DOM is adjusted as necessary.
  function domUpdate(newScene, currentScene) {
      ElmRuntime.draw(function(_) {
          Render.update(elm.node.firstChild, currentScene, newScene);
          if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();
      });
      return newScene;
  }
  var renderer = A3(Signal.foldp, F2(domUpdate), currentScene, signalGraph);

  // must check for resize after 'renderer' is created so
  // that changes show up.
  if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();

  return renderer;
}

}());
