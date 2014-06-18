
(function() {
'use strict';

ElmRuntime.Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

ElmRuntime.counter = 0;
ElmRuntime.guid = function() { return ElmRuntime.counter++; }

ElmRuntime.use = function(M) {
    if (typeof M === 'function') M = M();
    return M;
};

function isAlive(input) {
    if (!('defaultNumberOfKids' in input)) return true;
    var len = input.kids.length;
    if (len === 0) return false;
    if (len > input.defaultNumberOfKids) return true;
    var alive = false;
    for (var i = len; i--; ) {
        alive = alive || isAlive(input.kids[i]);
    }
    return alive;
}

ElmRuntime.filterDeadInputs = function(inputs) {
    var temp = [];
    for (var i = inputs.length; i--; ) {
        if (isAlive(inputs[i])) temp.push(inputs[i]);
    }
    return temp;
};

// define the draw function
var vendors = ['ms', 'moz', 'webkit', 'o'];
var win = typeof window !== 'undefined' ? window : {};
for (var i = 0; i < vendors.length && !win.requestAnimationFrame; ++i) {
    win.requestAnimationFrame = win[vendors[i]+'RequestAnimationFrame'];
    win.cancelAnimationFrame  = win[vendors[i]+'CancelAnimationFrame'] ||
                                win[vendors[i]+'CancelRequestAnimationFrame'];
}

if (win.requestAnimationFrame && win.cancelAnimationFrame) {
    var previous = 0;
    ElmRuntime.draw = function(callback) {
        win.cancelAnimationFrame(previous);
        previous = win.requestAnimationFrame(callback);
    };
} else {
    ElmRuntime.draw = function(callback) { callback(); };
}

// Binary Heap implementation from
// http://eloquentjavascript.net/appendix2.html
(function (ElmRuntime) {

function BinaryHeap(scoreFunction){
  this.content = [];
  this.scoreFunction = scoreFunction;
}

BinaryHeap.prototype = {
  push: function(element) {
    // Add the new element to the end of the array.
    this.content.push(element);
    // Allow it to bubble up.
    this.bubbleUp(this.content.length - 1);
  },

  pop: function() {
    // Store the first element so we can return it later.
    var result = this.content[0];
    // Get the element at the end of the array.
    var end = this.content.pop();
    // If there are any elements left, put the end element at the
    // start, and let it sink down.
    if (this.content.length > 0) {
    this.content[0] = end;
    this.sinkDown(0);
    }
    return result;
  },

  remove: function(node) {
    var length = this.content.length;
    // To remove a value, we must search through the array to find
    // it.
    for (var i = 0; i < length; i++) {
    if (this.content[i] != node) continue;
    // When it is found, the process seen in 'pop' is repeated
    // to fill up the hole.
    var end = this.content.pop();
    // If the element we popped was the one we needed to remove,
    // we're done.
    if (i == length - 1) break;
    // Otherwise, we replace the removed element with the popped
    // one, and allow it to float up or sink down as appropriate.
    this.content[i] = end;
    this.bubbleUp(i);
    this.sinkDown(i);
    break;
    }
  },

  size: function() {
    return this.content.length;
  },

  bubbleUp: function(n) {
    // Fetch the element that has to be moved.
    var element = this.content[n], score = this.scoreFunction(element);
    // When at 0, an element can not go up any further.
    while (n > 0) {
    // Compute the parent element's index, and fetch it.
    var parentN = Math.floor((n + 1) / 2) - 1,
        parent = this.content[parentN];
    // If the parent has a lesser score, things are in order and we
    // are done.
    if (score >= this.scoreFunction(parent))
        break;

    // Otherwise, swap the parent with the current element and
    // continue.
    this.content[parentN] = element;
    this.content[n] = parent;
    n = parentN;
    }
  },

  sinkDown: function(n) {
    // Look up the target element and its score.
    var length = this.content.length,
    element = this.content[n],
    elemScore = this.scoreFunction(element);

    while(true) {
    // Compute the indices of the child elements.
    var child2N = (n + 1) * 2, child1N = child2N - 1;
    // This is used to store the new position of the element,
    // if any.
    var swap = null;
    // If the first child exists (is inside the array)...
    if (child1N < length) {
        // Look it up and compute its score.
        var child1 = this.content[child1N],
        child1Score = this.scoreFunction(child1);
        // If the score is less than our element's, we need to swap.
        if (child1Score < elemScore)
        swap = child1N;
    }
    // Do the same checks for the other child.
    if (child2N < length) {
        var child2 = this.content[child2N],
        child2Score = this.scoreFunction(child2);
        if (child2Score < (swap == null ? elemScore : child1Score))
        swap = child2N;
    }

    // No need to swap further, we are done.
    if (swap == null) break;

    // Otherwise, swap and continue.
    this.content[n] = this.content[swap];
    this.content[swap] = element;
    n = swap;
    }
  }
}
ElmRuntime.BinaryHeap = BinaryHeap;
})(ElmRuntime);

}());
