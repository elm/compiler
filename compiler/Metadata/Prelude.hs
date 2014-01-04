{-# OPTIONS_GHC -W #-}
module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Paths_Elm as Path
import System.Exit
import System.IO
import SourceSyntax.Module

import qualified InterfaceSerialization as IS

add :: Bool -> Module def -> Module def
add noPrelude (Module name exs ims decls) = Module name exs (customIms ++ ims) decls
    where
      customIms = if noPrelude then [] else concatMap addModule prelude

      addModule (n, method) = case lookup n ims of
                                Nothing     -> [(n, method)]
                                Just (As _) -> [(n, method)]
                                Just _      -> []

prelude :: [(String, ImportMethod)]
prelude = text ++ map (\n -> (n, Hiding [])) modules
  where
    text = map ((,) "Text") [ As "Text", Hiding ["link", "color", "height"] ]
    modules = [ "Basics", "Signal", "List", "Maybe", "Time", "Prelude"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

interfaces :: Bool -> IO Interfaces
interfaces noPrelude =
    do ifaces <- safeReadDocs =<< Path.getDataFileName "interfaces.data"
       return $ if noPrelude then Map.empty else ifaces

safeReadDocs :: FilePath -> IO Interfaces
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      let _ = err :: IOError
      putStrLn $ unlines [ "Error reading types for standard library!"
                         , "    The file should be at " ++ name
                         , "    If you are using a stable version of Elm,"
                         , "    please report an issue at github.com/evancz/Elm"
                         , "    and specify your versions of Elm and your OS" ]
      exitFailure

hasInterfaces :: [(String, ModuleInterface)] -> Either String [(String, ModuleInterface)]
hasInterfaces [] = Left "No interfaces found in serialized Prelude!"
hasInterfaces ifaces = Right ifaces

readDocs :: FilePath -> IO Interfaces
readDocs filePath = do
  bytes <- IS.loadInterface filePath
  let interfaces = IS.interfaceDecode filePath =<< bytes
  case mapM (IS.validVersion filePath) =<< hasInterfaces =<< interfaces of
    Right ifaces -> return $ Map.fromList ifaces
    Left err -> do
      hPutStrLn stderr err
      exitFailure
