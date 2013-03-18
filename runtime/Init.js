
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
      var newElement = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
      baseNode = newElement('div');
      document.body.appendChild(baseNode);
      baseNode.style.width  = window.innerWidth + 'px';
      baseNode.style.height = window.innerHeight + 'px';
      window.addEventListener('resize', function() {
	      console.log('resize the base node');
	      baseNode.style.width  = window.innerWidth + 'px';
	      baseNode.style.height = window.innerHeight + 'px';
	  }, true);

      var style = newElement('style');
      style.type = 'text/css';
      style.innerHTML = "html,head,body { padding:0; margin:0; }" +
	  "body { font-family: calibri, helvetica, arial, sans-serif; }";
      document.head.appendChild(style);
  }

  // create the actuall RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {notify: notify, node: baseNode, id: ElmRuntime.guid(), inputs: inputs};

  // If graphics are enabled, set up the `main` value in baseNode.
  if (baseNode !== null) {
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
    baseNode.appendChild(Render.render(visualModel));

    if ('Window' in elm) {
      var w = baseNode.clientWidth;
      if (w !== elm.Window.dimensions.value._0) {
	  notify(elm.Window.dimensions.id,
		 Elm.Native.Utils(elm).Tuple2(w, baseNode.clientHeight));
      }
    }
  
    // set up updates so that the DOM is adjusted as necessary.
    var update = Render.update;
    function domUpdate(value) {
      update(baseNode.firstChild, visualModel, value);
      visualModel = value;
      return value;
    }

    signalGraph = A2(Signal.lift, domUpdate, signalGraph);
  }

  function send(name, value) {
      var e = document.createEvent('Event');
      e.initEvent(name + '_' + elm.id, true, true);
      e.value = value;
      document.dispatchEvent(e);
  }
  function recv(name, handler) {
      document.addEventListener(name + '_' + elm.id, handler);
  }

  return { send : send, recv : recv };
};
