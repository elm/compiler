npm install elm [![Travis build Status](https://travis-ci.org/elm-lang/elm-platform.svg?branch=master)](http://travis-ci.org/elm-lang/elm-platform) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/6mcub79i04ianpm9/branch/master?svg=true)](https://ci.appveyor.com/project/rtfeldman/elm-platform/branch/master)
===============

Install the [Elm Platform](https://github.com/elm-lang/elm-platform) via [`npm`](https://www.npmjs.com).

## Installing

Run this to get the binaries:

```
$ npm install -g elm
```

## Installing behind a proxy server

If you are behind a proxy server, set the environment variable "HTTPS_PROXY".

```
$ export HTTPS_PROXY=$YourProxyServer$
$ npm install -g elm
```

Or on Windows:

```
$ set HTTPS_PROXY=$YourProxyServer$
$ npm install -g elm
```

## Troubleshooting

1. [Troubleshooting npm](https://github.com/npm/npm/wiki/Troubleshooting)
2. On Debian/Ubuntu systems, you may have to install the nodejs-legacy package: `apt-get install nodejs-legacy`.
3. If the installer says that it cannot find any usable binaries for your operating system and architecture, check the [Build from Source](https://github.com/elm-lang/elm-platform/blob/master/README.md#build-from-source) documentation.

## Getting Started

Once everything has installed successfully, head over to the [Get Started](http://elm-lang.org/Get-Started.elm) page!
