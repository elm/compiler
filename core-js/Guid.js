
Elm = {};

var Guid = function() {
 var counter = 0;
 var guid = function() { counter += 1; return counter; };
 return {guid : guid};
}();