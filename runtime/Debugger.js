
(function() {
'use strict';

var staticDebugger = null;

Elm.debuggerInit = function(module) {
  return function(elm) {
    staticDebugger = debuggerInit(module, elm);
    return staticDebugger.moduleRetValue;
  };
};

Elm.debuggerReset = function() {
  staticDebugger.reset();
}

Elm.debuggerRecord = function() {
  staticDebugger.startRecording();
}

Elm.debuggerPlayback = function() {
  staticDebugger.stopRecording();
}


function debuggerInit(module, elm) {
  var State = { OFF: 0, RECORDING: 1, PLAYBACK: 2 };
  var debuggerState = State.RUNNING;

  var allNodes = null;
  var initialProgramState = null;
  var recordedEvents = [];
  var asyncTimers = [];

  function wrapNotify(id, v) {
    var timestep = Date.now();
    if (debuggerState != State.PLAYBACK) {
      elm.notify(id, v, timestep);
    }
    if (debuggerState == State.RECORDING) {
      recordEvent(id, v, timestep);
    }
  };

  function wrapRunDelayed(func, delayMs) {
    return safeSetTimeout(func, delayMs);
  };

  function safeSetTimeout(func, delayMs) {
    var timerId = setTimeout(func, delayMs);
    asyncTimers.push(timerId);
    return timerId;
  }

  function recordEvent(id, v, timestep) {
    recordedEvents.push([id, v, timestep]);
  }

  function resetProgram() {
    clearAsyncEvents();
    restoreProgramState(allNodes, initialProgramState);
    redrawGraphics();
  }

  function startRecording() {
    clearAsyncEvents();
    restoreProgramState(allNodes, initialProgramState);
    debuggerState = State.RECORDING;
    recordedEvents = [];
  }

  function stopRecording() {
    clearAsyncEvents();
    restoreProgramState(allNodes, initialProgramState);
    debuggerState = State.PLAYBACK;
    doPlayback(recordedEvents);
  }

  function clearAsyncEvents() {
    asyncTimers.forEach(function(id) {
      clearTimeout(id);
    });
  }

  function doPlayback(eventList) {
    var x = eventList.shift();
    var time = x[2];
    elm.notify(x[0], x[1], x[2]);

    if (eventList.length > 0) {
      var delta = eventList[0][2] - time;
      setTimeout(function() { doPlayback(eventList); }, delta);
    }
  }

  function redrawGraphics() {
    var graphicsFoldp = moduleRetValue.main.kids[0];
    graphicsFoldp.recv(Date.now(), true, moduleRetValue.main.id);
  }

  var wrappedElm = {
    notify: wrapNotify,
    runDelayed: wrapRunDelayed,
    node: elm.node,
    display: elm.display,
    id: elm.id,
    addListener: elm.addListener,
    inputs: elm.inputs
  };

  var moduleRetValue = module(wrappedElm);

  var elmDebugger = {
        moduleRetValue: moduleRetValue,

        // debugger functions
        startRecording: startRecording,
        stopRecording: stopRecording,
        reset: resetProgram
  };

  allNodes = flattenNodes(wrappedElm.inputs);
  initialProgramState = saveProgramState(allNodes);

  return elmDebugger;
};



function assert(bool, msg) {
  if (!bool) {
    throw "Assertion error: " + msg;
  }
}

function saveProgramState(allNodes) {
  var nodeValues = [];

  allNodes.forEach(function(node) {
    nodeValues.push({ value: node.value, id: node.id });
  });

  return nodeValues;
};


function restoreProgramState(allNodes, nodeStates) {
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
