var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
// Use major.minor.patch from version string - e.g. "1.2.3" from "1.2.3-alpha"
var binVersion = packageInfo.version.replace(/^(\d+\.\d+\.\d+).*$/, "$1");

var root =
  "https://github.com/elm/compiler/releases/download/" +
  binVersion +
  "/binaries-for-";

module.exports = binwrap({
  dirname: __dirname,
  binaries: ["elm"],
  urls: {
    "darwin-x64": root + "mac.tar.gz",
    "win32-x64": root + "windows.tar.gz",
    "win32-ia32": root + "windows.tar.gz",
    "linux-x64": root + "linux.tar.gz"
  }
});
