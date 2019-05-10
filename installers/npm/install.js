
var download = require('./download.js');
var package = require('./package.json');
var path = require('path');


// get slashes right for Windows
var targetPath = path.resolve(__dirname, Object.values(package.bin)[0]);

download(targetPath, function() {});
