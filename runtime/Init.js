
Elm.init = function(module, baseNode) {
  'use strict';

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

  // ensure that baseNode exists and is properly formatted.
  if (typeof baseNode === 'undefined') {
      baseNode = document.body;
      var style = document.createElement('style');
      style.type = 'text/css';
      style.innerHTML = "html,head,body { padding:0; margin:0; }" +
                        "body { font-family: calibri, helvetica, arial, sans-serif; }";
      document.head.appendChild(style);
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {notify: notify, node: baseNode, id: ElmRuntime.guid(), inputs: inputs};

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

  recv('elm_log', function(e) {console.log(e.value);});
  recv('elm_title', function(e) {document.title = e.value;});
  recv('elm_redirect', function(e) {
    if (e.value.length > 0) { window.location = e.value; }
  });

  // If graphics are not enabled, escape early, skip over setting up DOM stuff.
  if (baseNode === null) return { send : send, recv : recv };


  var Render = ElmRuntime.use(ElmRuntime.Render.Element);

  // evaluate the given module and extract its 'main' value.
  signalGraph = module(elm).main;

  // make sure the signal graph is actually a signal, extract the visual model,
  // and filter out any unused inputs.
  var Signal = Elm.Signal(elm);
  if (!('recv' in signalGraph)) signalGraph = Signal.constant(signalGraph);
  visualModel = signalGraph.value;
  inputs = ElmRuntime.filterDeadInputs(inputs);

  var tuple2 = Elm.Native.Utils(elm).Tuple2;
  function adjustWindow() {
      if ('Window' in elm) {
          var w = baseNode.clientWidth;
          if (w !== elm.Window.dimensions.value._0) {
              notify(elm.Window.dimensions.id,
                     tuple2(w, document.body === baseNode ?
                            window.innerHeight : baseNode.clientHeight));
          }
      }
  }

  // Add the visualModel to the DOM
  var renderNode = Render.render(visualModel);
  baseNode.appendChild(renderNode);
  adjustWindow();

  // set up updates so that the DOM is adjusted as necessary.
  var update = Render.update;
  function domUpdate(value) {
      ElmRuntime.draw(function(_) {
              update(renderNode, visualModel, value);
              visualModel = value;
              adjustWindow();
          });
      return value;
  }

  signalGraph = A2(Signal.lift, domUpdate, signalGraph);

  return { send : send, recv : recv };
};
