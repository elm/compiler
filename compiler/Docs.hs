{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import System.IO

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text

import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Declaration as D

import Text.Parsec hiding (newline,spaces)
import Parse.Declaration (alias,datatype,infixDecl)
import Parse.Expression (typeAnnotation)
import Parse.Helpers
import Parse.Module (moduleDef)

data Flags = Flags
    { files :: [FilePath] }
    deriving (Data,Typeable,Show,Eq)

defaultFlags :: Flags
defaultFlags = Flags
  { files = def &= args &= typ "FILES"
  } &= help "Generate documentation for Elm"
    &= summary ("Generate documentation for Elm, (c) Evan Czaplicki")

main :: IO ()
main = do
  flags <- cmdArgs defaultFlags
  mapM_ parseFile (files flags)

config :: Config
config = Config { confIndent = 2, confCompare = keyOrder keys }
  where
    keys = ["tag","name","document","comment","raw","aliases","datatypes"
           ,"values","typeVariables","type","constructors"]

parseFile :: FilePath -> IO ()
parseFile path = do
  source <- readFile path
  case iParse docs source of
    Right json -> do
      putStrLn $ "Documenting " ++ path
      let docPath = "docs" </> replaceExtension path ".json"
      createDirectoryIfMissing True (dropFileName docPath)
      BS.writeFile docPath (encodePretty' config json)
    Left err -> do
      hPutStrLn stderr $ "Parse error in " ++ path ++ " at " ++ show err
      exitFailure

docs :: IParser Value
docs = do
  (name, exports, structure) <- moduleDocs
  things <- document
  return $ documentToJson name exports structure things

docComment :: IParser String
docComment = do
  try (string "{-|")
  contents <- closeComment
  let reversed = dropWhile (`elem` " \n\r") . drop 2 $ reverse contents
  return $ dropWhile (==' ') (reverse reversed)

moduleDocs :: IParser (String, [String], String)
moduleDocs = do
  optional freshLine
  (names,exports) <- moduleDef
  manyTill (string " " <|> newline <?> "more whitespace")
           (lookAhead (string "{-|") <?> "module documentation comment")
  structure <- docComment
  return (List.intercalate "." names, exports, structure)

document :: IParser [(String, D.ParseDeclaration, String)]
document = onFreshLines (\t ts -> ts ++ [t]) [] docThing

docThing :: IParser (String, D.ParseDeclaration, String)
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
        (src,def) <- withSource $ choice [ alias, datatype, D.Definition <$> typeAnnotation ]
        return (comment, def, src)


documentToJson name exports structure things =
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
                | all Help.isSymbol name = addInfix (D.L, 9 :: Int) pairs
                | otherwise = pairs

            customOps = Map.intersectionWith addInfix infixes types
            addInfix (assoc,prec) pairs =
                [ "associativity" .= show assoc, "precedence" .= prec ] ++ pairs

      (comment, decl, source) : rest ->
          case decl of
            D.Fixity assoc prec name ->
                collect (Map.insert name (assoc,prec) infixes) types aliases adts rest
            D.Definition (E.TypeAnnotation name tipe) ->
                collect infixes (insert name [ "type" .= tipe ] types) aliases adts rest
            D.TypeAlias name vars tipe ->
                let fields = ["typeVariables" .= vars, "type" .= tipe ]
                in  collect infixes types (insert name fields aliases) adts rest
            D.Datatype name vars ctors ->
                let tipe = T.Data name (map T.Var vars)
                    fields = ["typeVariables" .= vars
                             , "constructors" .= map (ctorToJson tipe) ctors ]
                in  collect infixes types aliases (insert name fields adts) rest
          where
            insert name fields dict = Map.insert name (obj name fields) dict
            obj name fields =
                [ "name" .= name, "raw" .= source, "comment" .= comment ] ++ fields

instance ToJSON T.Type where
  toJSON tipe =
    object $
    case tipe of
      T.Lambda _ _ ->
          let tipes = T.collectLambdas tipe in
          [ "tag" .= ("function" :: Text.Text)
          , "args" .= toJSON (init tipes)
          , "result" .= toJSON (last tipes)
          ]

      T.Var x ->
          [ "tag" .= ("var" :: Text.Text)
          , "name" .= toJSON x
          ]
      
      T.Data name ts -> 
          [ "tag" .= ("adt" :: Text.Text)
          , "name" .= toJSON name
          , "args" .= map toJSON ts
          ]
       
      T.Record fields ext ->
          [ "tag" .= ("record" :: Text.Text)
          , "fields" .= toJSON (map (toJSON . second toJSON) fields)
          , "extension" .= toJSON ext
          ]

ctorToJson :: T.Type -> (String, [T.Type]) -> Value
ctorToJson tipe (ctor, tipes) =
    object [ "name" .= ctor
           , "type" .= foldr T.Lambda tipe tipes ]
