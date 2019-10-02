# Installing on Windows

The installer for Windows is available [here](https://guide.elm-lang.org/install.html).


<br/>

## Uninstall

First run the `C:\Program Files (x86)\Elm\0.19\uninstall.exe` file. This will remove Elm stuff from your `PATH`.

Then remove the whole `C:\Users\<username>\AppData\Roaming\elm` directory. Elm caches some packages and build artifacts to reduce compile times and to help you work offline. Getting rid of this directory will clear that information out!

<br/>

## Building the Windows installer

You will need the [NSIS installer](http://nsis.sourceforge.net/Download) to be installed.

Once everything is installed, run something like this command:

    make_installer.cmd 0.19.0

It will build an installer called `Elm-0.19.0-setup.exe`.
