# Building Windows installer

You will need the [NSIS installer](http://nsis.sourceforge.net/Download) to be installed.

Once everything is installed, run something like this command:

    make_installer.cmd 0.17

It will build an installer called `ElmPlatform-0.17-setup.exe`. You can read
more about what arguments can be given to this command in the comments in
`BuildFromSource.hs`.