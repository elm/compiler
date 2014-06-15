{-# OPTIONS_GHC -W #-}
module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Control.Exception as E
import System.Exit
import System.IO
import AST.Module as Module
import qualified Build.Interface as Interface
import Build.Utils (getDataFile)

add :: Bool -> Module exs body -> Module exs body
add noPrelude (Module name path exs ims decls) =
    Module name path exs (customIms ++ ims) decls
    where
      customIms = if noPrelude then [] else concatMap addModule prelude

      addModule (n, method) = case lookup n ims of
                                Nothing     -> [(n, method)]
                                Just (As _) -> [(n, method)]
                                Just _      -> []

prelude :: [(String, ImportMethod)]
prelude = text : string : map (\n -> (n, Module.open)) modules
  where
    string = ("String", Module.importing ["show"])
    modules = [ "Basics", "Signal", "List", "Maybe", "Time", "Color"
              , "Graphics.Element", "Graphics.Collage"
              , "Native.Ports", "Native.Json"
              ]
    text = ("Text", Module.importing textImports)
    textImports =
        [ "toText", "leftAligned", "rightAligned", "centered", "justified"
        , "plainText", "asText", "typeface", "monospace", "bold", "italic" 
        ]

interfaces :: Bool -> IO Interfaces
interfaces noPrelude =
    if noPrelude
    then return Map.empty
    else safeReadDocs =<< getDataFile "interfaces.data"

safeReadDocs :: FilePath -> IO Interfaces
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      let _ = err :: IOError
      hPutStrLn stderr $ unlines $
         [ "Error reading types for standard library from file " ++ name
         , "    If you are using a stable version of Elm, please report an issue at"
         , "    <http://github.com/elm-lang/Elm/issues> specifying version numbers for"
         , "    Elm and your OS." ]
      exitFailure

readDocs :: FilePath -> IO Interfaces
readDocs filePath = do
  interfaces <- Interface.load filePath
  case mapM (Interface.isValid filePath) (interfaces :: [(String, Module.Interface)]) of
    Left err -> do
      hPutStrLn stderr err
      exitFailure

    Right [] -> do
      hPutStrLn stderr "No interfaces found in serialized Prelude!"
      exitFailure

    Right ifaces -> return $ Map.fromList ifaces
