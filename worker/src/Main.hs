{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Control.Monad (msum)
import qualified Data.ByteString as BS
import Snap.Core
import Snap.Http.Server

import qualified Artifacts
import qualified Cors
import qualified Endpoint.Compile as Compile
import qualified Endpoint.Repl as Repl



-- RUN THE DEV SERVER


main :: IO ()
main =
  do  rArtifacts <- Artifacts.loadRepl
      cArtifacts <- Artifacts.loadCompile
      errorJS <- Compile.loadErrorJS
      let depsInfo = Artifacts.toDepsInfo cArtifacts

      httpServe config $ msum $
        [ ifTop $ status
        , path "repl" $ Repl.endpoint rArtifacts
        , path "compile" $ Compile.endpoint cArtifacts
        , path "compile/errors.js" $ serveJavaScript errorJS
        , path "compile/deps-info.json" $ serveDepsInfo depsInfo
        , notFound
        ]


config :: Config Snap a
config =
  setPort 8000 $ setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig


status :: Snap ()
status =
  do  modifyResponse $ setContentType "text/plain"
      writeBuilder "Status: OK"


notFound :: Snap ()
notFound =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder "Not Found"


serveJavaScript :: BS.ByteString -> Snap ()
serveJavaScript javascript =
  do  modifyResponse $ setContentType "application/javascript"
      writeBS javascript


serveDepsInfo :: BS.ByteString -> Snap ()
serveDepsInfo json =
  Cors.allow GET ["https://elm-lang.org"] $
    do  modifyResponse $ setContentType "application/json"
        writeBS json
