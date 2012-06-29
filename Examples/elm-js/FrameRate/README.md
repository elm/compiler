
Use the arrow keys to move the square around.


To compile this example yourself use:

    elm --import-js="FrameRateHelp.js" FrameRate.elm

This compiles the Elm file and includes the necessary JavaScript code.
It produces a self-contained HTML file.


Note: Not all browsers like reading the elm-runtime-x.y.z.js file from
an absolute path, so you may have to specify a relative path with
the --runtime flag (e.g. --runtime="../../../elm/elm-runtime-0.3.5.js").
