{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import Text.RawString.QQ (r)



-- SANDWICH


sandwich :: Name.Name -> B.Builder -> B.Builder
sandwich moduleName javascript =
  let name = Name.toBuilder moduleName in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>|] <> name <> [r|</title>
</head>

<body>
<div id="elm-df98db9626"></div>
<script>
|] <> javascript <> [r|

var app = Elm.|] <> name <> [r|.init({ node: document.getElementById("elm-df98db9626") });
if (document.getElementById("elm-df98db9626"))
{
  document.getElementById("elm-df98db9626").innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.';
}
</script>
</body>
</html>|]
