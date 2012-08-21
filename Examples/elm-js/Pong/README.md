

To compile this example yourself use:

    elm --import-js="FrameRateHelp.js" Pong.elm

This compiles the Elm file and includes the necessary JavaScript code.
It produces a self-contained HTML file called Pong.html.


Note: Not all browsers like reading the elm-runtime-x.y.z.js file from
an absolute path, so you may have to specify a relative path with
the --runtime flag (e.g. adding the following to your compile command:
`--runtime="../../../elm/elm-runtime-0.3.6.js"`).
