module InterfaceSerialization ( loadInterface
                              , interfaceDecode
                              , validVersion
                              ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as Binary

import Paths_Elm (version)
import Data.Version (showVersion)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import SourceSyntax.Module

loadInterface :: FilePath -> IO (Either String L.ByteString)
loadInterface filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      byteString <- L.readFile filePath
      return $ Right byteString

    else
      return $ Left $ "Unable to find file " ++ filePath ++
                      " for deserialization!"

interfaceDecode :: Binary.Binary a =>
                   FilePath -> L.ByteString -> Either String a
interfaceDecode filePath bytes = do
  case Binary.decodeOrFail bytes of
    Right (_, _, binaryInfo) -> Right binaryInfo

    Left (_, offset, err) ->
        Left $ concat [ "Got an error, '" ++ err ++ "' at offset "
                      , show offset ++ " of " ++ filePath ++ ".\n\n"
                      , "This error may be due to an outdated or corrupt "
                      , "artifact from a previous build. Please rebuild "
                      , filePath ++ " and try again."
                      ]

validVersion :: FilePath -> (String, ModuleInterface) ->
                Either String (String, ModuleInterface)
validVersion filePath (name, interface) =
    if iVersion interface == showVersion version then
        Right (name, interface)
    else
        Left $ "Found build artifacts created by a different " ++
               "Elm compiler version. Please rebuild " ++
               filePath ++ " and try again."
