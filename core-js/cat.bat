copy /B Guid.js+foreign\JavaScript.js+foreign\JSON.js+Value.js+List.js+Data.js+collage\Color.js+Element.js+Text.js+collage\Shape.js+collage\Line.js+runtime\Dispatcher.js+runtime\Signal.js+Prelude.js+Everything.js ..\elm-mini.js

cd ..\elm

copy /B ..\elm-mini.js elm-runtime-0.4.0.js

cabal install
