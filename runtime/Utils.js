
(function() {
'use strict';

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
for (var i = 0; i < vendors.length && !window.requestAnimationFrame; ++i) {
    window.requestAnimationFrame = window[vendors[i]+'RequestAnimationFrame'];
    window.cancelAnimationFrame  = window[vendors[i]+'CancelAnimationFrame'] ||
                                   window[vendors[i]+'CancelRequestAnimationFrame'];
}

if (window.requestAnimationFrame && window.cancelAnimationFrame) {
    var previous = 0;
    ElmRuntime.draw = function(callback) {
        window.cancelAnimationFrame(previous);
        previous = window.requestAnimationFrame(callback);
    };
} else {
    ElmRuntime.draw = function(callback) { callback(); };
}

}());
