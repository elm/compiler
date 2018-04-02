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
  header <> javascript <> footer (N.toBuilder moduleName)


header :: B.Builder
header = [r|
<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
</head>

<body>
<div id="embed"></div>
<script>
|]


footer :: B.Builder -> B.Builder
footer name =
  "</script>\n\
  \<script>\n\
  \// It is too complicated to figure out which kind of Program you created\n\
  \// based on only the code. So here we check for different possibilities.\n\
  \//\n\
  \// When you embed compiled Elm into HTML yourself, you should not copy/paste\n\
  \// from here. Do just the part that makes sense for your application!\n\
  \//\n\
  \var app =\n\
  \    Elm." <> name <> ".fullscreen\n\
  \        ? Elm." <> name <> ".fullscreen()\n\
  \        :\n\
  \    Elm." <> name <> ".embed\n\
  \        ? Elm." <> name <> ".embed(document.getElementById('embed'))\n\
  \        :\n\
  \    Elm." <> name <> ".worker\n\
  \        ? (document.getElementById('embed').innerText = 'This is a headless program, meaning there is nothing to show here. I started the program anyway though, and you can access it as `app` in the developer console.', Elm." <> name <> ".worker())\n\
  \        : (document.getElementById('embed').innerText = 'I cannot figure out what kind of program this is supposed to be. Perhaps someone on Slack can help?', undefined);\n\
  \</script>\n\
  \</body>\n\
  \</html>"
