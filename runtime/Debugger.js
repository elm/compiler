
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
  var graphicsNode = debugModule.moduleInstance.main.kids[0];
  
  var tracePath = tracePathInit(runtime);
  var tracePathNode = A2(Signal.lift, tracePath.graphicsUpdate, graphicsNode);
  runtime.node.parentNode.appendChild(tracePath.canvas);

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

function Point(x, y) {
  this.x = x;
  this.y = y;

  this.translate = function(x, y) {
    return new Point(this.x + x, this.y + y);
  }

  this.equals = function(p) {
    return this.x == p.x && this.y == p.y;
  }
}

function tracePathInit(runtime) {
  var List = Elm.List.make(runtime);
  var tracePathCanvas = createCanvas();
  var tracePositions = {};

  function findPositions(currentScene) {
    var positions = {};
    function processElement(elem, offset) {
      if (elem.element.ctor == "Custom" && elem.element.type == "Collage")
      {
        List.map(F2(processForm)(offset))(elem.element.model.forms);
      }
      else if (elem.element.ctor == "Image" && elem.props.debugTracePathId)
      {
        positions[elem.props.debugTracePathId] = new Point(offset.x, offset.y);
      }
    }

    function processForm(offset, form) {
      if (form.form.ctor == "FElement")
      {
        processElement(form.form._0, offset.translate(form.x, -form.y));
      }
    }

    processElement(currentScene, new Point(0, 0));
    return positions;
  }

  function appendPositions(positions) {
    for (var id in positions) {
      var pos = positions[id];
      if (tracePositions.hasOwnProperty(id)) {
        var points = tracePositions[id];
        if (!pos.equals(points[points.length-1])) {
          points.push(pos);
        }
      }
      else {
        tracePositions[id] = [pos];
      }
    }
  }

  function graphicsUpdate(currentScene) {
    var ctx = tracePathCanvas.getContext('2d');
    ctx.clearRect(0, 0, tracePathCanvas.width, tracePathCanvas.height);
    ctx.fillText("Debugger trace path", 20, 20);

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    appendPositions(findPositions(currentScene));
    for (var id in tracePositions)
    {
      ctx.beginPath();
      var points = tracePositions[id];
      for (var i=0; i < points.length; i++)
      {
        var p = points[i];
        if (i == 0) {
          ctx.moveTo(p.x, p.y);
        }
        else {
          ctx.lineTo(p.x, p.y);
        }
      }
    }
    ctx.lineWidth = 1;
    ctx.strokeStyle = "black";
    ctx.stroke();

    ctx.restore();
  }

  function clearTraces() {
    tracePositions = {};
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces
  }
}


function createCanvas() {
  var c = document.createElement('canvas');
  c.width = window.innerWidth;
  c.height = window.innerHeight;
  c.style.position = "absolute";
  c.style.top = "0";
  c.style.left = "0";
  return c;
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
