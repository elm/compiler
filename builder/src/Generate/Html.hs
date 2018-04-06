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
<div id="embed"></div>
<script>
|] <> javascript <> [r|

// It is too complicated to figure out which kind of Program you created
// based on only the code. So here we check for different possibilities.
//
// When you embed compiled Elm into HTML yourself, you should not copy/paste
// from here. Do just the part that makes sense for your application!
//
var app =
    Elm.|] <> name <> [r|.fullscreen
        ? Elm.|] <> name <> [r|.fullscreen()
        :
    Elm.|] <> name <> [r|.embed
        ? Elm.|] <> name <> [r|.embed(document.getElementById('embed'))
        :
    Elm.|] <> name <> [r|.worker
        ? (document.getElementById('embed').innerText = 'This is a headless program, meaning there is nothing to show here. I started the program anyway though, and you can access it as `app` in the developer console.', Elm.|] <> name <> [r|.worker())
        : (document.getElementById('embed').innerText = 'I cannot figure out what kind of program this is supposed to be. Perhaps someone on Slack can help?', undefined);
</script>
</body>
</html>|]
