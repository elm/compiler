(function() {

// Returns boolean indicating if the swap was successful.
// Requires that the two signal graphs have exactly the same
// structure.
ElmRuntime.swap = function(from, to) {
    function similar(nodeOld,nodeNew) {
        return nodeOld.kids.length === nodeNew.kids.length;
    }
    function swap(nodeOld,nodeNew) {
        nodeNew.value = nodeOld.value;
        return true;
    }
    from_ = from.__private__;
    to_ = to.__private__;
    var canSwap = depthFirstTraversals(similar, from_.inputs, to_.inputs);
    if (canSwap) {
        depthFirstTraversals(swap, from_.inputs, to_.inputs);
        to.send("(!@#$%^&*)", null);
    }
    from_.container.parentNode.replaceChild(to_.container, from_.container);
    return canSwap;
}

// Returns false if the node operation f ever fails.
function depthFirstTraversals(f, queueOld, queueNew) {
    while (queueOld.length > 0 && queueNew.length > 0) {
        var nodeOld = queueOld.pop();
        var nodeNew = queueNew.pop();
        if (!f(nodeOld, nodeNew)) return false;
        queueOld = queueOld.concat(nodeOld.kids);
        queueNew = queueNew.concat(nodeNew.kids);
    }
    return queueOld.length === queueNew.length;
}

}())