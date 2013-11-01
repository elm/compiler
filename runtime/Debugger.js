
(function() {
'use strict';

Elm.Debugger = null;
Elm.debuggerAttach = function(module) {
  return {
    make: function(runtime) {
      var wrappedModule = debugModule(module, runtime);
      Elm.debuggerInit = function() {
        Elm.Debugger = debuggerInit(wrappedModule, runtime);
      };
      return wrappedModule.moduleInstance;
    }
  };
};

function debugModule(module, runtime) {
  var programPaused = false;
  var recordedEvents = [];
  var eventCounter = 0;
  var asyncTimers = [];

  function wrapNotify(id, v) {
    var timestep = Date.now();

    if (programPaused) {
      // ignore notify because we are stepping
    }
    else {
      runtime.notify(id, v, timestep);
      recordEvent(id, v, timestep);
    }
  };

  function wrapRunDelayed(func, delayMs) {
    return safeSetTimeout(func, delayMs);
  };

  function safeSetTimeout(func, delayMs) {
    var timerId = setTimeout(func, delayMs);
    asyncTimers.push({ func:func, delayMs:delayMs, timerId:timerId });
    return timerId;
  }

  function recordEvent(id, v, timestep) {
    recordedEvents.push({ id:id, value:v, timestep:timestep });
  }

  function clearAsyncEvents() {
    asyncTimers.forEach(function(timer) {
      clearTimeout(timer.timerId);
    });
  }

  function clearRecordedEvents() {
    recordedEvents = [];
  }

  function getRecordedEvents() {
    return recordedEvents.slice();
  }

  function setPaused(v) {
    programPaused = v;
  }

  function getPaused() {
    return programPaused;
  }

  var wrappedRuntime = {
    notify: wrapNotify,
    runDelayed: wrapRunDelayed,
    node: runtime.node,
    display: runtime.display,
    id: runtime.id,
    addListener: runtime.addListener,
    inputs: runtime.inputs
  };

  var moduleInstance = module.make(wrappedRuntime);
  var moduleNodes = flattenNodes(wrappedRuntime.inputs);

  return {
    moduleInstance: moduleInstance,
    moduleNodes: moduleNodes,
    initialNodeValues: saveNodeValues(moduleNodes),
    initialAsyncTimers: asyncTimers.slice(),
    // API functions
    clearAsyncEvents: clearAsyncEvents,
    clearRecordedEvents: clearRecordedEvents,
    getRecordedEvents: getRecordedEvents,
    getPaused: getPaused,
    setPaused: setPaused
  };
}

function debuggerInit(debugModule, runtime) {
  var Signal = Elm.Signal.make(runtime);
  var List = Elm.List.make(runtime);
  var graphicsNode = debugModule.moduleInstance.main.kids[0];
  
  var tracePathCanvas = document.createElement('canvas');
  tracePathCanvas.width = window.innerWidth;
  tracePathCanvas.height = window.innerHeight;
  tracePathCanvas.style.position = "absolute";
  tracePathCanvas.style.top = "0";
  tracePathCanvas.style.left = "0";
  runtime.node.parentNode.appendChild(tracePathCanvas);

  function graphicsUpdate(currentScene) {
    var ctx = tracePathCanvas.getContext('2d');
    ctx.clearRect(0, 0, tracePathCanvas.width, tracePathCanvas.height);
    ctx.fillText("Debugger trace path", 20, 20);

    function drawElement(elem) {
      if (elem.element.ctor == "Custom" && elem.element.type == "Collage")
      {
        List.map(drawForm)(elem.element.model.forms);
      }
      else if (elem.element.ctor == "Image")
      {
        var x = 0;
        var y = 0;
        ctx.beginPath();
        ctx.arc(x, y, 2, 0, 2*Math.PI);
        ctx.fillStyle = "grey";
        ctx.fill();
        ctx.lineWidth = 1;
        ctx.strokeStype = "black";
        ctx.stroke();
      }
    }

    function drawForm(form) {
      ctx.save();
      ctx.scale(form.scale, form.scale);
      ctx.rotate(form.theta);
      ctx.translate(form.x, -form.y);
      if (form.form.ctor == "FElement")
      {
        drawElement(form.form._0);
      }

      ctx.restore();
    }

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    drawElement(currentScene);
    ctx.restore();
  }

  var tracePathNode = A2(Signal.lift, graphicsUpdate, graphicsNode);

  function resetProgram() {
    debugModule.clearAsyncEvents();
    restoreNodeValues(debugModule.moduleNodes, debugModule.initialNodeValues);
    redrawGraphics();
  }

  function restartProgram() {
    resetProgram();
    debugModule.clearRecordedEvents();
    debugModule.initialAsyncTimers.forEach(function(timer) {
      var func = timer.func;
      func();
    });
  }

  function stepTo(index) {
    if (!debugModule.getPaused()) {
      debugModule.setPaused(true);
      resetProgram();
    }

    if (index < 0 || index >= recordedEvents.length) {
      throw "Index out of bounds: " + index;
    }

    if (index < eventCounter) {
      resetProgram();
      eventCounter = 0;
    }

    assert(index >= eventCounter, "index must be bad");
    while (eventCounter < index) {
      var nextEvent = recordedEvents[eventCounter];
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      eventCounter += 1;
    }
    assert(eventCounter == index, "while loop didnt' work");
  }

  function getMaxSteps() {
    return recordedEvents.length - 1;
  }

  function doPlayback(eventList) {
    var x = eventList.shift();
    var time = x[2];
    runtime.notify(x[0], x[1], x[2]);

    if (eventList.length > 0) {
      var delta = eventList[0][2] - time;
      setTimeout(function() { doPlayback(eventList); }, delta);
    }
  }

  function redrawGraphics() {
    graphicsNode.recv(Date.now(), true, debugModule.moduleInstance.main.id);
  }

  var elmDebugger = {
      reset: resetProgram,
      restart: restartProgram,
      getMaxSteps: getMaxSteps,
      stepTo: stepTo
  };

  return elmDebugger;
}



function assert(bool, msg) {
  if (!bool) {
    throw "Assertion error: " + msg;
  }
}

function saveNodeValues(allNodes) {
  var nodeValues = [];

  allNodes.forEach(function(node) {
    nodeValues.push({ value: node.value, id: node.id });
  });

  return nodeValues;
};


function restoreNodeValues(allNodes, nodeStates) {
  assert(allNodes.length == nodeStates.length, "saved program state has wrong length");
  for (var i=0; i < allNodes.length; i++) {
    var node = allNodes[i];
    var state = nodeStates[i];
    assert(node.id == state.id, "the nodes moved position");

    node.value = state.value;
  }
}

function flattenNodes(nodes) {
  var treeReduce = function(prev, cur, i, arr) {
    var flatKids = cur.kids.reduce(treeReduce, []);
    flatKids.push(cur);
    return prev.concat(flatKids);
  }

  return nodes.reduce(treeReduce, []);
};

}());
