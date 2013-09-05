{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS

import SourceSyntax.Helpers (isSymbol)
import SourceSyntax.Declaration (Declaration(..), Assoc(..))
import SourceSyntax.Expression (Def(..))

import Text.Parsec hiding (newline,spaces)
import Parse.Declaration (alias,datatype,infixDecl)
import Parse.Expression (typeAnnotation)
import Parse.Helpers
import Parse.Module (moduleDef)

data Flags = Flags
    { files :: [FilePath] }
    deriving (Data,Typeable,Show,Eq)

defaultFlags = Flags
  { files = def &= args &= typ "FILES"
  } &= help "Generate documentation for Elm"
    &= summary ("Generate documentation for Elm, (c) Evan Czaplicki")

main = do
  flags <- cmdArgs defaultFlags
  mapM parseFile (files flags)

parseFile path = do
  source <- readFile path
  case iParse docs "" source of
    Right json -> do
      putStrLn $ "Documenting " ++ path
      createDirectoryIfMissing True "docs"
      BS.writeFile ("docs" </> replaceExtension path ".json")
                   (encodePretty' (defConfig { confIndent = 2 }) json)
    Left err -> do
      putStrLn $ "Parse error in " ++ path ++ " at " ++ show err
      exitFailure

docs :: IParser Value
docs = do
  (name, exports, structure) <- moduleDocs
  things <- document
  return $ toJson name exports structure things

docComment :: IParser String
docComment = do
  try (string "{-|")
  contents <- closeComment
  return $ dropWhile (==' ') (init (init contents))

moduleDocs = do
  (names,exports) <- anyThen moduleDef
  anyThen (lookAhead (string "{-|"))
  structure <- docComment
  return (List.intercalate "." names, exports, structure)

document :: IParser [(String, Declaration t v, String)]
document = go []
    where
      go things = do
        thing <- optionMaybe docThing
        let things' = case thing of
                        Nothing -> things
                        Just t -> things ++ [t]
        done <- chompUntilFreshLine
        case done of
          True  -> return things'
          False -> go things'

-- returns whether the end of file has been reached
chompUntilFreshLine :: IParser Bool
chompUntilFreshLine =
    anyThen . choice $
       [ try (simpleNewline >> notFollowedBy (string " ")) >> return False
       , eof >> return True ]

docThing :: IParser (String, Declaration t v, String)
docThing = uncommentable <|> commented <|> uncommented ""
    where
      uncommentable = do
        ifx <- infixDecl
        return ("", ifx, "")

      commented = do
        comment <- docComment
        freshLine
        uncommented comment

      uncommented comment = do
        (src,def) <- withSource $ choice [ alias, datatype, Definition <$> typeAnnotation ]
        return (comment, def, src)


toJson name exports structure things =
    object $ [ "name"      .= name
             , "document"  .= structure
             , "values"    .= toList values
             , "aliases"   .= toList aliases
             , "datatypes" .= toList adts
             ]
  where
    (values, aliases, adts) = collect Map.empty Map.empty Map.empty Map.empty things
    
    toList dict = map object . Map.elems $ filterPublics dict

    exportMap = Map.fromList (zip exports exports)
    filterPublics dict =
        case Map.null exportMap of
          True -> dict
          False -> Map.filterWithKey (\k _ -> Map.member k exportMap) dict

collect infixes types aliases adts things =
    case things of
      [] -> (Map.union customOps nonCustomOps, aliases, adts)
          where
            nonCustomOps = Map.mapWithKey addDefaultInfix $ Map.difference types infixes
            addDefaultInfix name pairs
                | all isSymbol name = addInfix (L, 9 :: Int) pairs
                | otherwise = pairs

            customOps = Map.intersectionWith addInfix infixes types
            addInfix (assoc,prec) pairs =
                [ "associativity" .= show assoc, "precedence" .= prec ] ++ pairs

      (comment, decl, source) : rest ->
          case decl of
            Fixity assoc prec name ->
                collect (Map.insert name (assoc,prec) infixes) types aliases adts rest
            Definition (TypeAnnotation name tipe) ->
                collect infixes (insert name [] types) aliases adts rest
            TypeAlias name vars tipe ->
                collect infixes types (insert name ["vars" .= vars] aliases) adts rest
            Datatype name vars ctors ->
                collect infixes types aliases (insert name ["vars" .= vars] adts) rest
          where
            insert name fields dict = Map.insert name (obj name fields) dict
            obj name fields =
                [ "name" .= name, "raw" .= source, "comment" .= comment ] ++ fields