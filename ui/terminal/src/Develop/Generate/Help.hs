{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Develop.Generate.Help
  ( makePageHtml
  , makeCodeHtml
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Text.RawString.QQ (r)

import qualified Elm.Name as N


-- PAGES


makePageHtml :: N.Name -> B.Builder -> B.Builder
makePageHtml moduleName flags =
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <link type="text/css" rel="stylesheet" href="/_elm/styles.css">
  <script src="/_elm/elm.js"></script>
</head>
<body>
<script>
Elm.|] <> N.toBuilder moduleName <> [r|.init({ flags: |] <> flags <> [r| });
</script>
</body>
</html>
|]



-- CODE


makeCodeHtml :: FilePath -> B.Builder -> B.Builder
makeCodeHtml title code =
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>|] <> B.stringUtf8 title <> [r|</title>
  <style type="text/css">
    @import url(/_elm/source-code-pro.ttf);
    html, head, body, pre { margin: 0; height: 100%; }
    body { font-family: "Source Code Pro", monospace; }
  </style>
  <link type="text/css" rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/default.min.css">
  <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js"></script>
  <script>if (hljs) { hljs.initHighlightingOnLoad(); }</script>
</head>
<body style="background-color: #F0F0F0;">
<pre><code>|] <> code <> [r|</code></pre>
</body>
</html>
|]
