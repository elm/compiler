(function() {
'use strict';

if (typeof window != 'undefined' && !window.location.origin) {
  window.location.origin =
      window.location.protocol + "//" +
      window.location.hostname +
      (window.location.port ? (':' + window.location.port) : '');
}

Elm.fullscreenDebugHooks = function(elmModule, debuggerHistory /* =undefined */) {
  var exposedDebugger = {};
  function debuggerAttach(elmModule, debuggerHistory) {
    return {
      make: function(runtime) {
        var wrappedModule = debugModule(elmModule, runtime);
        exposedDebugger = debuggerInit(wrappedModule, runtime, debuggerHistory);
        return wrappedModule.debuggedModule;
      }
    }
  }
  var mainHandle = Elm.fullscreen(debuggerAttach(elmModule, debuggerHistory));
  mainHandle.debugger = exposedDebugger;
  return mainHandle;
};

var EVENTS_PER_SAVE = 100;

function debugModule(module, runtime) {
  var programPaused = false;
  var recordedEvents = [];
  var asyncCallbacks = [];
  var snapshots = [];
  var watchTracker = Elm.Native.Debug.make(runtime).watchTracker;
  var pauseTime = 0;
  var eventsUntilSnapshot = EVENTS_PER_SAVE;
  runtime.debuggerStatus = runtime.debuggerStatus || {};
  runtime.debuggerStatus.eventCounter = runtime.debuggerStatus.eventCounter || 0;

  // runtime is the prototype of wrappedRuntime
  // so we can access all runtime properties too
  var wrappedRuntime = Object.create(runtime);
  wrappedRuntime.notify = notifyWrapper;
  wrappedRuntime.setTimeout = setTimeoutWrapper;

  // make a copy of the wrappedRuntime
  var assignedPropTracker = Object.create(wrappedRuntime);
  var debuggedModule = module.make(assignedPropTracker);

  // make sure the signal graph is actually a signal & extract the visual model
  if ( !('recv' in debuggedModule.main) ) {
    debuggedModule.main = Elm.Signal.make(runtime).constant(debuggedModule.main);
  }

  // The main module stores imported modules onto the runtime.
  // To ensure only one instance of each module is created,
  // we assign them back on the original runtime object.
  Object.keys(assignedPropTracker).forEach(function(key) {
    runtime[key] = assignedPropTracker[key];
  });

  var signalGraphNodes = flattenSignalGraph(wrappedRuntime.inputs);
  var tracePath = tracePathInit(runtime, debuggedModule.main);

  snapshots.push(snapshotSignalGraph(signalGraphNodes));

  function notifyWrapper(id, v) {
    var timestep = runtime.timer.now();

    if (programPaused) {
      // ignore async events generated while playing back
      // or user events while program is paused
      return false;
    }
    else {
      recordEvent(id, v, timestep);
      var changed = runtime.notify(id, v, timestep);
      snapshotOnCheckpoint();
      if (parent.window) {
        parent.window.postMessage("elmNotify", window.location.origin);
      }
      return changed;
    }
  };

  function setTimeoutWrapper(func, delayMs) {
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
    runtime.debuggerStatus.eventCounter += 1;
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
    runtime.debuggerStatus.eventCounter = 0;
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

  function clearSnapshots() {
    snapshots = [snapshotSignalGraph(signalGraphNodes)];
  }

  function getSnapshotAt(i) {
    var snapshotEvent = Math.floor(i / EVENTS_PER_SAVE);
    assert(snapshotEvent < snapshots.length && snapshotEvent >= 0,
           "Out of bounds index: " + snapshotEvent);
    return snapshots[snapshotEvent];
  }

  function snapshotOnCheckpoint() {
    if (eventsUntilSnapshot === 1) {
      snapshots.push(snapshotSignalGraph(signalGraphNodes));
      eventsUntilSnapshot = EVENTS_PER_SAVE;
    } else {
      eventsUntilSnapshot -= 1;
    }
  }

  function setPaused() {
    programPaused = true;
    clearAsyncCallbacks();
    pauseTime = Date.now();
    tracePath.stopRecording();
    preventInputEvents();
  }

  function setContinue(position) {
    var pauseDelay = Date.now() - pauseTime;
    runtime.timer.addDelay(pauseDelay);
    programPaused = false;

    // we need to dump the events that are ahead of where we're continuing.
    var lastSnapshotPosition = Math.floor(position / EVENTS_PER_SAVE);
    eventsUntilSnapshot = EVENTS_PER_SAVE - (position % EVENTS_PER_SAVE);
    snapshots = snapshots.slice(0, lastSnapshotPosition + 1);

    if (position < getRecordedEventsLength()) {
      var lastEventTime = recordedEvents[position].timestep;
      var scrubTime = runtime.timer.now() - lastEventTime;
      runtime.timer.addDelay(scrubTime);
    }

    recordedEvents = recordedEvents.slice(0, position);
    tracePath.clearTracesAfter(position);
    runtime.debuggerStatus.eventCounter = position;
    executeCallbacks(asyncCallbacks);
    permitInputEvents();

    tracePath.startRecording();
  }

  function getPaused() {
    return programPaused;
  }

  function preventInputEvents(){
    var events =
      [ "click", "mousemove", "mouseup", "mousedown", "mouseclick"
      , "keydown", "keypress", "keyup", "touchstart", "touchend"
      , "touchcancel", "touchleave", "touchmove", "pointermove"
      , "pointerdown", "pointerup", "pointerover", "pointerout"
      , "pointerenter", "pointerleave", "pointercancel"
      ];

    var ignore = function(e) {
      var evt = e ? e : window.event;
      if (evt.stopPropagation) {
        evt.stopPropagation();
      }
      if (evt.cancelBubble !== null) {
        evt.cancelBubble = true;
      }
      if (evt.preventDefault) {
        evt.preventDefault();
      }
      return false;
    };

    var ignoringDiv = document.getElementById("elmEventIgnorer");
    if (!ignoringDiv) {
      ignoringDiv = document.createElement("div");
      ignoringDiv.id = "elmEventIgnorer";
      ignoringDiv.style.position = "absolute";
      ignoringDiv.style.top = "0px";
      ignoringDiv.style.left = "0px";
      ignoringDiv.style.width = "100%";
      ignoringDiv.style.height = "100%";

      for (var i = events.length; i-- ;) {
        ignoringDiv.addEventListener(events[i], ignore, true);
      }
      runtime.node.appendChild(ignoringDiv);
    }
  }

  function permitInputEvents(){
    var ignoringDiv = document.getElementById("elmEventIgnorer");
    ignoringDiv.parentNode.removeChild(ignoringDiv);
  }

  return {
    debuggedModule: debuggedModule,
    signalGraphNodes: signalGraphNodes,
    initialSnapshot: snapshotSignalGraph(signalGraphNodes),
    initialAsyncCallbacks: asyncCallbacks.slice(),
    // API functions
    clearAsyncCallbacks: clearAsyncCallbacks,
    clearRecordedEvents: clearRecordedEvents,
    getRecordedEventsLength: getRecordedEventsLength,
    getRecordedEventAt: getRecordedEventAt,
    copyRecordedEvents: copyRecordedEvents,
    loadRecordedEvents: loadRecordedEvents,
    clearSnapshots: clearSnapshots,
    getSnapshotAt: getSnapshotAt,
    snapshotOnCheckpoint: snapshotOnCheckpoint,
    getPaused: getPaused,
    setPaused: setPaused,
    setContinue: setContinue,
    tracePath: tracePath,
    watchTracker: watchTracker
  };
}

// The debuggerHistory variable is passed in on swap. It represents
// the a state of the debugger for it to assume during init. It contains
// the paused state of the debugger, the recorded events, and the current
// event being processed.
function debuggerInit(debugModule, runtime, debuggerHistory /* =undefined */) {
  var currentEventIndex = 0;

  function resetProgram(position) {
    var closestSnapshot = debugModule.getSnapshotAt(position);
    debugModule.clearAsyncCallbacks();
    restoreSnapshot(debugModule.signalGraphNodes, closestSnapshot);
    redrawGraphics();
  }

  function restartProgram() {
    pauseProgram();
    resetProgram(0);
    debugModule.watchTracker.clear();
    debugModule.tracePath.clearTraces();
    debugModule.setContinue(0);
    debugModule.clearRecordedEvents();
    debugModule.clearSnapshots();
    executeCallbacks(debugModule.initialAsyncCallbacks);
  }

  function pauseProgram() {
    debugModule.setPaused();
    currentEventIndex = debugModule.getRecordedEventsLength();
  }

  function continueProgram() {
    if (debugModule.getPaused())
    {
      var closestSnapshotIndex =
          Math.floor(currentEventIndex / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(currentEventIndex);
      var continueIndex = currentEventIndex;
      currentEventIndex = closestSnapshotIndex;
      stepTo(continueIndex);
      debugModule.setContinue(currentEventIndex);
    }
  }

  function stepTo(index) {
    if (!debugModule.getPaused()) {
      debugModule.setPaused();
      resetProgram();
    }

    if (index < 0 || index > getMaxSteps()) {
      throw "Index out of bounds: " + index;
    }

    if (index < currentEventIndex) {
      var closestSnapshotIndex = Math.floor(index / EVENTS_PER_SAVE) * EVENTS_PER_SAVE;
      resetProgram(index);
      currentEventIndex = closestSnapshotIndex;
    }

    while (currentEventIndex < index) {
      var nextEvent = debugModule.getRecordedEventAt(currentEventIndex);
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      currentEventIndex += 1;
    }
  }

  function getMaxSteps() {
    return debugModule.getRecordedEventsLength();
  }

  function redrawGraphics() {
    var main = debugModule.debuggedModule.main
    for (var i = main.kids.length ; i-- ; ) {
      main.kids[i].recv(runtime.timer.now(), true, main.id);
    }
  }

  function getSwapState() {
    var continueIndex = currentEventIndex;
    if (!debugModule.getPaused()) {
      continueIndex = getMaxSteps();
    }
    return {
      paused: debugModule.getPaused(),
      recordedEvents: debugModule.copyRecordedEvents(),
      currentEventIndex: continueIndex
    };
  }

  function dispose() {
    var parentNode = runtime.node.parentNode;
    parentNode.removeChild(debugModule.tracePath.canvas);
    parentNode.removeChild(runtime.node);
  }

  if (debuggerHistory) {
    // The problem is that we want to previous paused state. But
    // by the time JS reaches here, the old code has been swapped out
    // and the new modules are being generated. So we can ask the
    // debugging console what it thinks the pause state is and go
    // from there.
    var paused = debuggerHistory.paused;
    debugModule.setPaused();
    debugModule.loadRecordedEvents(debuggerHistory.recordedEvents);
    var index = getMaxSteps();
    runtime.debuggerStatus.eventCounter = 0;
    debugModule.tracePath.clearTraces();

    // draw new trace path
    debugModule.tracePath.startRecording();
    while(currentEventIndex < index) {
      var nextEvent = debugModule.getRecordedEventAt(currentEventIndex);
      runtime.debuggerStatus.eventCounter += 1;
      runtime.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);
      debugModule.snapshotOnCheckpoint();
      currentEventIndex += 1;
    }
    debugModule.tracePath.stopRecording();

    stepTo(debuggerHistory.currentEventIndex);
    if (!paused) {
      debugModule.setContinue(debuggerHistory.currentEventIndex);
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
      getSwapState: getSwapState,
      dispose: dispose,
      allNodes: debugModule.signalGraphNodes,
      watchTracker: debugModule.watchTracker
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

function tracePathInit(runtime, signalGraphMain) {
  var List = Elm.List.make(runtime);
  var Signal = Elm.Signal.make(runtime);
  var tracePathNode = A2(Signal.map, graphicsUpdate, signalGraphMain);
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
    }

    function processForm(offset, form) {
      if (form.form.ctor == "FElement")
      {
        processElement(form.form._0, offset.translate(form.x, -form.y));
      }
      if (form.form.ctor == "FGroup")
      {
        var newOffset = offset.translate(form.x, -form.y);
        List.map(F2(processForm)(newOffset))(form.form._1);
      }
      if (form.debugTracePathId)
      {
        positions[form.debugTracePathId] = new Point(form.x + offset.x, -form.y + offset.y);
      }
    }

    processElement(currentScene, new Point(0, 0));
    return positions;
  }

  function appendPositions(positions) {
    for (var id in positions) {
      var pos = positions[id];
      if (tracePositions.hasOwnProperty(id)) {
        tracePositions[id].push(pos);
      }
      else {
        tracePositions[id] = [pos];
      }
      if (tracePositions[id].length < runtime.debuggerStatus.eventCounter) {
        var padCount = runtime.debuggerStatus.eventCounter - tracePositions[id].length;
        var lastTracePosition = tracePositions[id][tracePositions[id].length - 1];
        for (var i = padCount; i--;) {
          tracePositions[id].push(lastTracePosition)
        }
      }
      assert(tracePositions[id].length === runtime.debuggerStatus.eventCounter,
             "We don't have a 1-1 mapping of trace positions to events");
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

  function clearTracesAfter(position) {
    var newTraces = {};
    for (var id in tracePositions) {
      newTraces[id] = tracePositions[id].slice(0,position);
    }
    tracePositions = newTraces;
  }

  return {
    graphicsUpdate: graphicsUpdate,
    canvas: tracePathCanvas,
    clearTraces: clearTraces,
    clearTracesAfter: clearTracesAfter,
    stopRecording: stopRecording,
    startRecording: startRecording
  }
}

function executeCallbacks(callbacks) {
  callbacks.forEach(function(timer) {
    if (!timer.executed) {
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

function snapshotSignalGraph(signalGraphNodes) {
  var nodeValues = [];

  signalGraphNodes.forEach(function(node) {
    nodeValues.push({ value: node.value, id: node.id });
  });

  return nodeValues;
};

function restoreSnapshot(signalGraphNodes, snapshot) {
  assert(signalGraphNodes.length == snapshot.length,
         "saved program state has wrong length");
  for (var i=0; i < signalGraphNodes.length; i++) {
    var node = signalGraphNodes[i];
    var state = snapshot[i];
    assert(node.id == state.id, "the nodes moved position");

    node.value = state.value;
  }
}

function flattenSignalGraph(nodes) {
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
