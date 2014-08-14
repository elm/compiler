{-# OPTIONS_GHC -W #-}
module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Exception as E
import System.Exit
import System.IO
import AST.Module as Module
import qualified AST.Variable as Var
import qualified Build.Interface as Interface
import Build.Utils (getDataFile)


-- DEFINITION OF THE PRELUDE

type ImportDict =
    Map.Map String ([String], Var.Listing Var.Value)

prelude :: ImportDict
prelude = Map.unions [ string, text, maybe, openImports ]
  where
    importing :: String -> [Var.Value] -> ImportDict
    importing name values =
        Map.singleton name ([], Var.Listing values False)

    openImports :: ImportDict
    openImports =
        Map.fromList $ map (\name -> (name, ([], Var.openListing))) $
        [ "Basics", "Signal", "List", "Time", "Color"
        , "Graphics.Element", "Graphics.Collage"
        , "Native.Ports", "Native.Json"
        ]

    maybe :: ImportDict
    maybe = importing "Maybe" [ Var.ADT "Maybe" Var.openListing ]

    string :: ImportDict
    string = importing "String" [Var.Value "show"]

    text :: ImportDict
    text = importing "Text" (Var.ADT "Text" (Var.Listing [] False) : values)
      where
        values =
            map Var.Value
            [ "toText", "leftAligned", "rightAligned", "centered", "justified"
            , "plainText", "asText", "typeface", "monospace", "bold", "italic"
            ]


-- ADDING PRELUDE TO A MODULE

add :: Bool -> Module exs body -> Module exs body
add noPrelude (Module moduleName path exports imports decls) =
    Module moduleName path exports ammendedImports decls
  where
    ammendedImports =
      importDictToList $
        foldr addImport (if noPrelude then Map.empty else prelude) imports

importDictToList :: ImportDict -> [(String, ImportMethod)]
importDictToList dict =
    concatMap toImports (Map.toList dict)
  where
    toImports (name, (qualifiers, listing@(Var.Listing explicits open))) =
        map (\qualifier -> (name, As qualifier)) qualifiers
        ++
        if open || not (null explicits)
          then [(name, Open listing)]
          else []

addImport :: (String, ImportMethod) -> ImportDict -> ImportDict
addImport (name, method) importDict =
    Map.alter mergeMethods name importDict
  where
    mergeMethods :: Maybe ([String], Var.Listing Var.Value)
                 -> Maybe ([String], Var.Listing Var.Value)
    mergeMethods entry =
      let (qualifiers, listing) =
              case entry of
                Nothing -> ([], Var.Listing [] False)
                Just v -> v
      in
          case method of
            As qualifier -> Just (qualifier : qualifiers, listing)
            Open newListing ->
                Just (qualifiers, mergeListings newListing listing)

    mergeListings (Var.Listing explicits1 open1) (Var.Listing explicits2 open2) =
        Var.Listing
          (Set.toList (Set.fromList (explicits1 ++ explicits2)))
          (open1 || open2)


-- EXTRACT INTERFACES FROM STATIC FILE

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
