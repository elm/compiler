#!/usr/bin/env node

var child_process = require('child_process');
var path = require('path');
var fs = require('fs');


// Some npm users enable --ignore-scripts (a good security measure) so
// they do not run the post-install hook and install.js does not run.
// Instead they will run this script.
//
// On Mac and Linux, we download the elm executable into the exact same
// location as this file. Since npm uses symlinks on these platforms,
// that means that the first run will invoke this file and subsequent
// runs will call the elm binary directly.
//
// On Windows, we must download a file named elm.exe for it to run properly.
// Instead of symlinks, npm creates two files:
//
//   - node_modules/.bin/elm (a bash file)
//   - node_modules/.bin/elm.cmd (a batch file)
//
// Both files specifically invoke `node` to run the file listed at package.bin,
// so there is no way around instantiating node for no reason on Windows. So
// the existsSync check is needed so that it is not downloaded more than once.


// figure out where to put the binary (calls path.resolve() to get path separators right on Windows)
//
var binaryPath = path.resolve(__dirname, 'elm') + (process.platform === 'win32' ? '.exe' : '');

// Run the command directly if possible, otherwise download and then run.
// This check is important for Windows where this file will be run all the time.
//
if (process.platform === 'win32')
{
	fs.existsSync(binaryPath)
		? runCommand()
		: require('../download.js')(runCommand);
}
else
{
	require('../download.js')(runCommand);
}


function runCommand()
{
	// Need double quotes and { shell: true } when there are spaces in the path on windows:
	// https://github.com/nodejs/node/issues/7367#issuecomment-229721296
	child_process
		.spawn('"' + binaryPath + '"', process.argv.slice(2), { stdio: 'inherit', shell: true })
		.on('exit', process.exit);
}
