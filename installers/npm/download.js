var fs = require('fs');
var package = require('./package.json');
var URL = require('url').URL;
var https = require('https');
var zlib = require('zlib');



// MAIN
//
// This function is used by install.js and by the bin/elm backup that gets
// called when --ignore-scripts is enabled. That's why install.js is so weird.


module.exports = function(destinationPath, callback)
{
	var version = package.version.replace(/^(\d+\.\d+\.\d+).*$/, '$1'); // turn '1.2.3-alpha' into '1.2.3'
	var os = { 'darwin': 'mac', 'win32': 'windows', 'linux': 'linux' }[process.platform];
	var arch = { 'x64': '64-bit', 'ia32': '32-bit' }[process.arch];

	download(
		process.platform === 'win32' ? destinationPath + '.exe' : destinationPath,
		'https://github.com/elm/compiler/releases/download/' + version + '/binary-for-' + os + '-' + arch + '.gz',
		callback
	);
};



// DOWNLOAD


function download(destinationPath, url, callback)
{
	requestWithRedirects(url, 10,
		function(error) {
			exitFailure(url, 'I ran into trouble while fetching ' + url + '\n\n' + error);
		},
		function(response) {
			if (response.statusCode == 404)
			{
				exitFailure(url, 'I got a "404 Not Found" trying to download from the following URL:\n'
					+ url
					+ '\n\nThis may mean you are trying to install on an unsupported platform. (e.g. some 32-bit systems?)'
				);
			}

			response.on('error', function() {
				exitFailure(url, 'Something went wrong while receiving the following URL:\n\n' + url);
			});

			var gunzip = zlib.createGunzip().on('error', function(error) {
				exitFailure(url, 'I ran into trouble decompressing the downloaded binary. It is saying:\n\n' + error);
			});
			var write = fs.createWriteStream(destinationPath, {
				encoding: 'binary',
				mode: 0o755
			}).on('finish', callback).on('error', function(error) {
				exitFailure(url, 'I had some trouble writing file to disk. It is saying:\n\n' + error);
			});

			response.pipe(gunzip).pipe(write);
		}
	);
}



// HANDLE REDIRECTS


function requestWithRedirects(url, maxRedirects, failure, success)
{
	var req = https.request(url);

	req.on('response', function(response) {
		// Check for redirects

		var redirectUrl = new URL(response.headers['location'], new URL(url).origin).href;

		if (typeof redirectUrl === 'string' && response.statusCode >= 300 && response.statusCode < 400)
		{
			return (maxRedirects <= 0)
				? failure(new Error('followed 10 redirects and gave up'))
				: requestWithRedirects(redirectUrl, maxRedirects - 1, failure, success);
		}
		else
		{
			return success(response);
		}
	});

	req.on('error', failure);

	req.end();
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
