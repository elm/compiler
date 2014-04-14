
(function() {
'use strict';

if (!window.location.origin) {
  window.location.origin =
      window.location.protocol + "//" +
      window.location.hostname +
      (window.location.port ? (':' + window.location.port) : '');
}

Elm.Debugger = null;
Elm.debuggerAttach = function(module, hotSwapState /* =undefined */) {
  return {
    make: function(runtime) {
      var wrappedModule = debugModule(module, runtime);
      Elm.Debugger = debuggerInit(wrappedModule, runtime, hotSwapState);
      dispatchElmDebuggerInit();
      return wrappedModule.moduleInstance;
    }
  };
};

function dispatchElmDebuggerInit() {
  if (parent.window) {
    var dispatch = function(_) {
      parent.window.postMessage("elmDebuggerInit", window.location.origin);
    };
    if (parent.window.document.readyState === "complete") {
      dispatch();
    }
    else {
      parent.window.addEventListener("load", dispatch);
    }
  }
}

function debugModule(module, runtime) {
  var programPaused = false;
  var recordedEvents = [];
  var asyncCallbacks = [];
  var watchTracker = Elm.Native.Debug.make(runtime).watchTracker;

  function wrapNotify(id, v) {
    var timestep = Date.now();

    if (programPaused) {
      // ignore async events generated while playing back
      // or user events while program is paused
      return false;
    }
    else {
      var changed = runtime.notify(id, v, timestep);
      recordEvent(id, v, timestep);
      if (parent.window) {
        parent.window.postMessage("elmNotify", window.location.origin);
      }
      return changed;
    }
  };

  function wrapRunDelayed(func, delayMs) {
    var cbObj = { func:func, delayMs:delayMs, timerId:0, executed:false };
    var timerId = setTimeout(function() {
        cbObj.executed = true;
        func();
      }, delayMs);
    cbObj.timerId = timerId;
    asyncCallbacks.push(cbObj);
    return timerId;
  }

  function recordEvent(id, v, timestep) {
    watchTracker.pushFrame();
    recordedEvents.push({ id:id, value:v, timestep:timestep });
  }

  function clearAsyncCallbacks() {
    asyncCallbacks.forEach(function(timer) {
      if (!timer.executed) {
        clearTimeout(timer.timerId);
      }
    });
  }

  function clearRecordedEvents() {
    recordedEvents = [];
  }

  function getRecordedEventsLength() {
    return recordedEvents.length;
  }

  function getRecordedEventAt(i) {
    return recordedEvents[i];
  }

  function copyRecordedEvents() {
    return recordedEvents.slice();
  }

  function loadRecordedEvents(events) {
    recordedEvents = events.slice();
  }

  function setPaused(v) {
    programPaused = v;
    if (programPaused) {
      clearAsyncCallbacks();
      tracePath.stopRecording();
    }
    else {
      // executeCallbacks should only be called when continuing, not when restarting.
      //executeCallbacks(asyncCallbacks, false);
      tracePath.startRecording();
    }
  }

  function getPaused() {
    return programPaused;
  }

  // runtime is the prototype of wrappedRuntime
  // so we can access all runtime properties too
  var wrappedRuntime = Object.create(runtime);
  wrappedRuntime.notify = wrapNotify;
  wrappedRuntime.runDelayed = wrapRunDelayed;

  var assignedPropTracker = Object.create(wrappedRuntime);
  var moduleInstance = module.make(assignedPropTracker);
  
  // make sure the signal graph is actually a signal & extract the visual model
  if ( !('recv' in moduleInstance.main) ) {
      moduleInstance.main = Elm.Signal.make(runtime).constant(moduleInstance.main);
  }

  // The main module stores imported modules onto the runtime.
  // To ensure only one instance of each module is created,
  // we assign them back on the original runtime object.
  Object.keys(assignedPropTracker).forEach(function(key) {
    runtime[key] = assignedPropTracker[key];
  });

  var moduleNodes = flattenNodes(wrappedRuntime.inputs);
  var tracePath = tracePathInit(runtime, moduleInstance.main);

  return {
    moduleInstance: moduleInstance,
    moduleNodes: moduleNodes,
    initialNodeValues: saveNodeValues(moduleNodes),
    initialAsyncCallbacks: asyncCallbacks.slice(),
    // API functions
    clearAsyncCallbacks: clearAsyncCallbacks,
    clearRecordedEvents: clearRecordedEvents,
    getRecordedEventsLength: getRecordedEventsLength,
    getRecordedEventAt: getRecordedEventAt,
    copyRecordedEvents: copyRecordedEvents,
    loadRecordedEvents: loadRecordedEvents,
    getPaused: getPaused,
    setPaused: setPaused,
    tracePath: tracePath,
    watchTracker: watchTracker,
  };
}

function debuggerInit(debugModule, runtime, hotSwapState /* =undefined */) {
  var eventCounter = 0;

  function resetProgram() {
    debugModule.clearAsyncCallbacks();
    restoreNodeValues(debugModule.moduleNodes, debugModule.initialNodeValues);
    redrawGraphics();
  }

  function restartProgram() {
    resetProgram();
    debugModule.watchTracker.clear();
    debugModule.tracePath.clearTraces();
    debugModule.clearRecordedEvents();
    debugModule.setPaused(false);
    executeCallbacks(debugModule.initialAsyncCallbacks, true);
  }

  function pauseProgram() {
    debugModule.setPaused(true);
    eventCounter = debugModule.getRecordedEventsLength();
  }

  function continueProgram() {
    if (debugModule.getPaused())
    {
      debugModule.setPaused(false);
    }
  }

  function stepTo(index) {
    if (!debugModule.getPaused()) {
      debugModule.setPaused(true);
      resetProgram();
    }

    if (index < 0 || index > getMaxSteps()) {
      throw "Index out of bounds: " + index;
    }

    if (index < eventCounter) {
      resetProgram();
      eventCounter = 0;
    }

    assert(index >= eventCounter, "index must be bad");
    while (eventCounter < index) {
      var nextEvent = debugModule.getRecordedEventAt(eventCounter);
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      eventCounter += 1;
    }
    assert(eventCounter == index, "while loop didn't work");
  }

  function getMaxSteps() {
    return debugModule.getRecordedEventsLength();
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
    var main = debugModule.moduleInstance.main
    for (var i = main.kids.length ; i-- ; ) {
      main.kids[i].recv(Date.now(), true, main.id);
    }
  }

  function getHotSwapState() {
    var counter = eventCounter;
    if (!debugModule.getPaused()) {
      counter = getMaxSteps();
    }
    return {
      recordedEvents: debugModule.copyRecordedEvents(),
      eventCounter: counter
    };
  }

  function dispose() {
    var parentNode = runtime.node.parentNode;
    parentNode.removeChild(debugModule.tracePath.canvas);
    parentNode.removeChild(runtime.node);
  }

  if (hotSwapState) {
    debugModule.setPaused(true);
    debugModule.loadRecordedEvents(hotSwapState.recordedEvents);

    // draw new trace path
    debugModule.tracePath.startRecording();
    stepTo(getMaxSteps());
    debugModule.tracePath.stopRecording();

    stepTo(hotSwapState.eventCounter);
  }

  runtime.node.parentNode.appendChild(debugModule.tracePath.canvas);

  var elmDebugger = {
      restart: restartProgram,
      pause: pauseProgram,
      kontinue: continueProgram,
      getMaxSteps: getMaxSteps,
      stepTo: stepTo,
      getPaused: debugModule.getPaused,
      getHotSwapState: getHotSwapState,
      dispose: dispose,
      allNodes: debugModule.moduleNodes,
      watchTracker: debugModule.watchTracker,
      mainNode: debugModule.moduleInstance.main
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

function tracePathInit(runtime, mainNode) {
  var List = Elm.List.make(runtime);
  var Signal = Elm.Signal.make(runtime);
  var tracePathNode = A2(Signal.lift, graphicsUpdate, mainNode);
  var tracePathCanvas = createCanvas();
  var tracePositions = {};
  var recordingTraces = true;

  function findPositions(currentScene) {
    var positions = {};
    function processElement(elem, offset) {
      if (elem.element.ctor == "Custom" && elem.element.type == "Collage")
      {
        List.map(F2(processForm)(offset))(elem.element.model.forms);
      }
      if (elem.props.debugTracePathId)
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
    if (!recordingTraces) {
      return;
    }

    var ctx = tracePathCanvas.getContext('2d');
    ctx.clearRect(0, 0, tracePathCanvas.width, tracePathCanvas.height);

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
      ctx.lineWidth = 1;
      ctx.strokeStyle = "rgba(50, 50, 50, 0.4)";
      ctx.stroke();

      for (var i=0; i < points.length; i++)
      {
        var p = points[i];
        ctx.beginPath();
        ctx.arc(p.x, p.y, 2, 0, Math.PI * 2);
        ctx.fillStyle = "rgba(50, 50, 50, 0.4)";
        ctx.fill();
      }
    }

    ctx.restore();
  }

  function clearTraces() {
    tracePositions = {};
  }

  function stopRecording() {
    recordingTraces = false;
  }

  function startRecording() {
    recordingTraces = true;
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces,
    stopRecording: stopRecording,
    startRecording: startRecording
  }
}


function executeCallbacks(callbacks, reexecute) {
  callbacks.forEach(function(timer) {
    if (reexecute || !timer.executed) {
      var func = timer.func;
      func();
    }
  });
}

function createCanvas() {
  var c = document.createElement('canvas');
  c.width = window.innerWidth;
  c.height = window.innerHeight;
  c.style.position = "absolute";
  c.style.top = "0";
  c.style.left = "0";
  c.style.pointerEvents = "none";
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
  var nodesById = {};

  function addAllToDict(node) {
    nodesById[node.id] = node;
    node.kids.forEach(addAllToDict);
  }
  nodes.forEach(addAllToDict);

  var allNodes = Object.keys(nodesById).sort().map(function(key) {
    return nodesById[key];
  });
  return allNodes;
};

}());
