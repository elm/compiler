
To compile this example yourself use:

    elm --import-js="LogHelp.js" Log.elm

This compiles the Elm file and includes the necessary JavaScript code.
It produces a self-contained HTML file.


You can see the logged messages in the developer console of your browser.
The keyboard shortcut is F12 in many browsers.


Note: Not all browsers like reading the elm-runtime-x.y.z.js file from
an absolute path, so you may have to specify a relative path with
the --runtime flag (e.g. --runtime="../../../elm/elm-runtime-0.3.5.js").
