
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
  var nodeSaveStates = [];
  var EVENTS_PER_SAVE = 100;
  var watchTracker = Elm.Native.Debug.make(runtime).watchTracker;
  var now = 0;
  var eventsBeforeSave = EVENTS_PER_SAVE;

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

  nodeSaveStates.push(saveNodeValues(moduleNodes));

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
      if ((eventsBeforeSave--) === 1) { // save a "quickjump" every EVENTS_PER_SAVE states
        saveState();
        eventsBeforeSave = EVENTS_PER_SAVE;
      }
      if (parent.window) {
        parent.window.postMessage("elmNotify", window.location.origin);
      }
      return changed;
    }
  };

  function wrapRunDelayed(func, delayMs) {
    if (programPaused) {
      // Don't push timers and such to the callback stack while we're paused.
      // It causes too many callbacks to be fired during unpausing.
      return 0;
    }
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

  function clearSaveStates() {
    nodeSaveStates = [saveNodeValues(moduleNodes)];
  }

  function getSaveStatesLength() {
    return nodeSaveStates.length;
  }

  function getSaveStateAt(i) {
    var savePosition = (i / EVENTS_PER_SAVE)|0;
    assert(savePosition < nodeSaveStates.length && savePosition >= 0, "Out of bounds index: " + savePosition);
    return nodeSaveStates[savePosition];
  }

  function saveState() {
    nodeSaveStates.push(saveNodeValues(moduleNodes));
  }

  function setPaused() {
    programPaused = true;
    clearAsyncCallbacks();
    tracePath.stopRecording();
    now = Date.now();
  }

  function setContinue(position) {
    var timerDelay = Date.now() - now;
    runtime.timer.addDelay(timerDelay);
    programPaused = false;
    if (position > 0) {
      // If we're not unpausing at the head, then we need to dump the
      // events that are ahead of where we're continuing.
      var lastSaveNode = ((position / EVENTS_PER_SAVE)|0) + 1;
      eventsBeforeSave = EVENTS_PER_SAVE - (position % EVENTS_PER_SAVE);
      nodeSaveStates = nodeSaveStates.slice(0, lastSaveNode);
      recordedEvents = recordedEvents.slice(0, position);
      executeCallbacks(asyncCallbacks, false);
    }
    tracePath.startRecording();
  }

  function getPaused() {
    return programPaused;
  }

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
    clearSaveStates : clearSaveStates,
    getSaveStatesLength: getSaveStatesLength,
    getSaveStateAt: getSaveStateAt,
    saveState: saveState,
    EVENTS_PER_SAVE: EVENTS_PER_SAVE,
    getPaused: getPaused,
    setPaused: setPaused,
    setContinue: setContinue,
    tracePath: tracePath,
    watchTracker: watchTracker
  };
}

function debuggerInit(debugModule, runtime, hotSwapState /* =undefined */) {
  var eventCounter = 0;

  function resetProgram(position) {
    var closestSaveState = debugModule.getSaveStateAt(position);
    var eventsPerSave = debugModule.EVENTS_PER_SAVE;
    debugModule.clearAsyncCallbacks();
    restoreNodeValues(debugModule.moduleNodes, closestSaveState);
    redrawGraphics();
  }

  function restartProgram() {
    resetProgram(0);
    debugModule.watchTracker.clear();
    debugModule.tracePath.clearTraces();
    debugModule.clearRecordedEvents();
    debugModule.clearSaveStates();
    debugModule.setContinue(0);
    executeCallbacks(debugModule.initialAsyncCallbacks, true);
  }

  function pauseProgram() {
    debugModule.setPaused();
    eventCounter = debugModule.getRecordedEventsLength();
  }

  function continueProgram() {
    if (debugModule.getPaused())
    {
      if(eventCounter === 0) {
        restartProgram();
        return;
      }
      var lastEvent = debugModule.getRecordedEventAt(eventCounter);
      var continueTime = lastEvent ? lastEvent.timestep : Date.now();
      var eventsPerSave = debugModule.EVENTS_PER_SAVE;
      var index = eventCounter;

      debugModule.tracePath.stopRecording();
      resetProgram(eventCounter);
      debugModule.tracePath.clearTracesAfter(continueTime);
      
      eventCounter = ((eventCounter / eventsPerSave)|0) * eventsPerSave
      stepTo(index);
      debugModule.setContinue(eventCounter);
    }
  }

  function stepTo(index) {
    if (!debugModule.getPaused()) {
      debugModule.setPaused();
      resetProgram(0);
    }

    if (index < 0 || index > getMaxSteps()) {
      throw "Index out of bounds: " + index;
    }

    if (index < eventCounter) {
      var eventsPerSave = debugModule.EVENTS_PER_SAVE;
      resetProgram(index);
      eventCounter = ((index / eventsPerSave)|0) * eventsPerSave;
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
    // The problem is that we want to previous paused state. But
    // by the time JS reaches here, the old code has been swapped out
    // and the new modules are being generated. So we can ask the
    // debugging console what it thinks the pause state is and go
    // from there.
    debugModule.setPaused();
    resetProgram(0);
    debugModule.loadRecordedEvents(hotSwapState.recordedEvents);
    var paused = top.debug.paused;
    var index = getMaxSteps();
    eventCounter = 0;
    var eventsBeforeSave = debugModule.EVENTS_PER_SAVE;

    // draw new trace path
    debugModule.tracePath.startRecording();
    while (eventCounter < index) {
      var nextEvent = debugModule.getRecordedEventAt(eventCounter);
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);
      if ((eventsBeforeSave--) === 1) {
        debugModule.saveState();
        eventsBeforeSave = debugModule.EVENTS_PER_SAVE;
      }
      eventCounter += 1;
    }
    debugModule.tracePath.stopRecording();

    stepTo(hotSwapState.eventCounter);
    if (!paused) {
      debugModule.setContinue(hotSwapState.eventCounter);
    }
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

function Point(x, y, timestamp) {
  this.x = x;
  this.y = y;
  this.timestamp = timestamp

  this.translate = function(x, y) {
    return new Point(this.x + x, this.y + y, timestamp);
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
        positions[elem.props.debugTracePathId] = new Point(offset.x, offset.y, Date.now());
      }
    }

    function processForm(offset, form) {
      if (form.form.ctor == "FElement")
      {
        processElement(form.form._0, offset.translate(form.x, -form.y));
      }
    }
    processElement(currentScene, new Point(0, 0, Date.now()));
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

  function clearTracesAfter(time) {
    var newTraces = {};
    for (var id in tracePositions) {
      newTraces[id] = [];
      for (var i = 0; i < tracePositions[id].length; i++) {
        if (tracePositions[id][i].timestamp <= time) {
          newTraces[id].push(tracePositions[id][i]);
        }
      }
    }
    tracePositions = newTraces;
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces,
    stopRecording: stopRecording,
    startRecording: startRecording,
    clearTracesAfter: clearTracesAfter
  }
}


function executeCallbacks(callbacks, reexecute) {
  callbacks.forEach(function(timer) {
    if (reexecute || !timer.executed) {
      var func = timer.func;
      timer.executed = true;
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
