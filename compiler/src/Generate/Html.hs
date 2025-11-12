{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
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
  <title>|] <> name <> [r|</title>
  <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre id="elm"></pre>

<script>
try {
|] <> javascript <> [r|

  var app = Elm.|] <> name <> [r|.init({ node: document.getElementById("elm") });
}
catch (e)
{
  // display initialization errors (e.g. bad flags, infinite recursion)
  var header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  var pre = document.getElementById("elm");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
</script>

</body>
</html>|]
