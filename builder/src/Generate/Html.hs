{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Text.RawString.QQ (r)

import qualified Elm.Name as N



-- SANDWICH


sandwich :: N.Name -> B.Builder -> B.Builder
sandwich moduleName javascript =
  let name = N.toBuilder moduleName in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>|] <> name <> [r|</title>
</head>

<body>
<div id="elm-f0111bc4e658d0f98db96260c16f7e49"></div>
<script>
|] <> javascript <> [r|

var app = Elm.|] <> name <> [r|.init({ node: document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49") });
if (document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49"))
{
  document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49").innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.';
}
</script>
</body>
</html>|]
