var fs = require('fs');
var package = require('./package.json');
var path = require('path');
var request = require('request');
var zlib = require('zlib');



// MAIN
//
// This function is used by install.js and by the bin/elm backup that gets
// called when --ignore-scripts is enabled. That's why install.js is so weird.


module.exports = function(callback)
{
	// figure out URL of binary
	var version = package.version.replace(/^(\d+\.\d+\.\d+).*$/, '$1'); // turn '1.2.3-alpha' into '1.2.3'
	var os = { 'darwin': 'mac', 'win32': 'windows', 'linux': 'linux' }[process.platform];
	var arch = { 'x64': '64-bit', 'ia32': '32-bit' }[process.arch];
	var url = 'https://github.com/elm/compiler/releases/download/' + version + '/binary-for-' + os + '-' + arch + '.gz';

	reportDownload(version, url);

	// figure out where to put the binary (calls path.resolve() to get path separators right on Windows)
	var binaryPath = path.resolve(__dirname, package.bin.elm) + (process.platform === 'win32' ? '.exe' : '');

	// set up handler for request failure
	function reportDownloadFailure(error)
	{
		exitFailure(url,'Something went wrong while fetching the following URL:\n\n' + url + '\n\nIt is saying:\n\n' + error);
	}

	// set up decompression pipe
	var gunzip = zlib.createGunzip().on('error', function(error) {
		exitFailure(url, 'I ran into trouble decompressing the downloaded binary. It is saying:\n\n' + error);
	});

	// set up file write pipe
	var write = fs.createWriteStream(binaryPath, {
		encoding: 'binary',
		mode: 0o755
	}).on('finish', callback).on('error', function(error) {
		exitFailure(url, 'I had some trouble writing file to disk. It is saying:\n\n' + error);
	});

	// put it all together
	request(url).on('error', reportDownloadFailure).pipe(gunzip).pipe(write);
}



// EXIT FAILURE


function exitFailure(url, message)
{
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ message
		+ '\n\nNOTE: You can avoid npm entirely by downloading directly from:\n'
		+ url + '\nAll this package does is download that file and put it somewhere.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// REPORT DOWNLOAD


function reportDownload(version, url)
{
	console.log(
		'--------------------------------------------------------------------------------\n\n'
		+ 'Downloading Elm ' + version + ' from GitHub.'
		+ '\n\nNOTE: You can avoid npm entirely by downloading directly from:\n'
		+ url + '\nAll this package does is download that file and put it somewhere.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
}
