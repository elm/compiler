
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
};

Elm.debuggerRestart = function() {
  staticDebugger.restart();
};

Elm.debuggerGetMaxSteps = function() {
  return staticDebugger.getMaxSteps();
};

Elm.debuggerStepTo = function(index) {
  staticDebugger.stepTo(index);
};

function debuggerInit(module, elm) {

  var programPaused = false;
  var allNodes = null;

  var recordedEvents = [];
  var eventCounter = 0;
  var asyncTimers = [];
  var initialProgramState = {
    nodeValues: [],
    asyncTimers: []
  };

  function wrapNotify(id, v) {
    var timestep = Date.now();

    if (programPaused) {
      // ignore notify because we are stepping
    }
    else {
      elm.notify(id, v, timestep);
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

  function resetProgram() {
    clearAsyncEvents();
    restoreNodeValues(allNodes, initialProgramState.nodeValues);
    redrawGraphics();
  }

  function restartProgram() {
    resetProgram();
    recordedEvents = [];
    initialProgramState.asyncTimers.forEach(function(timer) {
      var func = timer.func;
      func();
    });
  }

  function gotoStep(index) {
    if (!programPaused) {
      programPaused = true;
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
      elm.notify(nextEvent.id, nextEvent.value, nextEvent.timestep);

      eventCounter += 1;
    }
    assert(eventCounter == index, "while loop didnt' work");
  }

  function getMaxSteps() {
    return recordedEvents.length - 1;
  }

  function clearAsyncEvents() {
    asyncTimers.forEach(function(timer) {
      clearTimeout(timer.timerId);
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
        reset: resetProgram,
        restart: restartProgram,
        getMaxSteps: getMaxSteps,
        stepTo: gotoStep
  };

  allNodes = flattenNodes(wrappedElm.inputs);
  initialProgramState.nodeValues = saveNodeValues(allNodes);
  initialProgramState.asyncTimers = asyncTimers.slice();

  return elmDebugger;
};



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
