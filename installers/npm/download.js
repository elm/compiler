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
		destinationPath,
		'https://github.com/elm/compiler/releases/download/' + version + '/binary-for-' + os + '-' + arch + '.gz',
		callback
	);
};



// DOWNLOAD


function download(destinationPath, url, callback)
{
	requestWithRedirects(url, 10,
		function(error) {
			var url = host + '/' + path;
			throw new Error('Error fetching binary from ' + url + ': ' + error);
		},
		function(response) {
			if (response.statusCode == 404)
			{
				// TODO give specific error message
				throw new Error('Not Found: ' + url);
			}

			response.on('error', function() {
				throw new Error('Error receiving ' + url);
			});

			var gunzip = zlib.createGunzip().on('error', function(error) {
				throw new Error('Error decompressing elm.gz: ' + error);
			});
			var write = fs.createWriteStream(destinationPath, {
				encoding: 'binary',
				mode: 0o755
			}).on('finish', callback).on('error', function(error) {
				throw new Error('Error writing file: ' + error);
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

		if (typeof redirectPath === 'string' && response.statusCode >= 300 && response.statusCode < 400)
		{
			return (maxRedirects <= 0)
				? failure(new Error('followed 10 redirects and gave up'))
				: requestWithRedirects(redirectPath, maxRedirects - 1, failure, success);
		}
		else
		{
			return success(response);
		}
	});

	req.on('error', failure);

	req.end();
}
