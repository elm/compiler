{-# OPTIONS_GHC -W #-}
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
import Data.Aeson.Types (Pair)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text

import qualified AST.Helpers as Help
import qualified AST.Type as T
import qualified AST.Declaration as D
import qualified AST.Expression.Source as Src
import qualified AST.Variable as Var

import Text.Parsec hiding (newline,spaces)
import Parse.Declaration (typeDecl,infixDecl)
import Parse.Expression (typeAnnotation)
import Parse.Helpers
import qualified Parse.Module as Module

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
  (name, _exports, structure) <- moduleDocs
  things <- document
  return $ documentToJson name structure things

docComment :: IParser String
docComment = do
  try (string "{-|")
  contents <- closeComment
  let reversed = dropWhile (`elem` " \n\r") . drop 2 $ reverse contents
  return $ dropWhile (==' ') (reverse reversed)

moduleDocs :: IParser (String, Var.Listing Var.Value, String)
moduleDocs = do
  optional freshLine
  (names,exports) <- Module.header
  manyTill (string " " <|> newline <?> "more whitespace")
           (lookAhead (string "{-|") <?> "module documentation comment")
  structure <- docComment
  return (List.intercalate "." names, exports, structure)

document :: IParser [(String, D.SourceDecl, String)]
document = onFreshLines (\t ts -> ts ++ [t]) [] docThing

docThing :: IParser (String, D.SourceDecl, String)
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
        (src,def) <- withSource $ choice [ typeDecl, D.Definition <$> typeAnnotation ]
        return (comment, def, src)


documentToJson :: String -> String -> [(String, D.SourceDecl, String)] -> Value
documentToJson name structure things =
    object
        [ "name"      .= name
        , "document"  .= structure
        , "values"    .= toList values
        , "aliases"   .= toList aliases
        , "datatypes" .= toList adts
        ]
  where
    (values, aliases, adts) =
        collect Map.empty Map.empty Map.empty Map.empty things
    
    toList :: Map.Map String [Pair] -> [Value]
    toList dict =
        map object (Map.elems dict)

type Pairs = Map.Map String [Pair]

collect :: Map.Map String (D.Assoc, Int) -> Pairs -> Pairs -> Pairs
        -> [(String, D.SourceDecl, String)]
        -> (Pairs, Pairs, Pairs)
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
          let insert name fields dict = Map.insert name (obj name fields) dict
              obj name fields =
                  [ "name" .= name
                  , "raw" .= source
                  , "comment" .= comment
                  ] ++ fields
          in
          case decl of
            D.Definition def ->
                case def of
                  Src.TypeAnnotation name tipe ->
                      collect infixes (insert name [ "type" .= tipe ] types) aliases adts rest

                  Src.Definition _ _ ->
                      collect infixes types aliases adts rest

            D.Datatype name vars ctors ->
                let tipe = T.App (T.Type (Var.Raw name)) (map T.Var vars)
                    fields = [ "typeVariables" .= vars
                             , "constructors" .= map (ctorToJson tipe) ctors
                             ]
                in  collect infixes types aliases (insert name fields adts) rest

            D.TypeAlias name vars tipe ->
                let fields = [ "typeVariables" .= vars, "type" .= tipe ]
                in  collect infixes types (insert name fields aliases) adts rest

            D.Port _ ->
                collect infixes types aliases adts rest

            D.Fixity assoc prec name ->
                collect (Map.insert name (assoc,prec) infixes) types aliases adts rest


instance Var.ToString var => ToJSON (T.Type var) where
  toJSON tipe =
      object (getFields tipe)
    where
      getFields tipe =
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

            T.Type name ->
                [ "tag" .= ("adt" :: Text.Text)
                , "name" .= toJSON (Var.toString name)
                , "args" .= ([] :: [()])
                ]

            T.App (T.Type name) ts -> 
                [ "tag" .= ("adt" :: Text.Text)
                , "name" .= toJSON (Var.toString name)
                , "args" .= map toJSON ts
                ]

            T.App _ _ -> error "This is an unexpected error with elm-doc, please report it at <https://github.com/elm-lang/Elm/issues>"
             
            T.Record fields ext ->
                [ "tag" .= ("record" :: Text.Text)
                , "fields" .= toJSON (map (toJSON . second toJSON) fields)
                , "extension" .= toJSON ext
                ]

            T.Aliased _ t ->
                getFields t

ctorToJson :: T.RawType -> (String, [T.RawType]) -> Value
ctorToJson tipe (ctor, tipes) =
    object [ "name" .= ctor
           , "type" .= foldr T.Lambda tipe tipes
           ]
