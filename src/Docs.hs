{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import System.IO

import Control.Applicative ((<$>))
import Control.Arrow (second)
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Declaration as Decl
import qualified AST.Expression.Source as Source
import qualified AST.Variable as Var

import Text.Parsec hiding (newline, spaces)
import qualified Parse.Declaration as Parse (typeDecl, infixDecl)
import qualified Parse.Expression as Parse (typeAnnotation)
import qualified Parse.Helpers as Parse
import qualified Parse.Module as Parse (header)
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type.Extract as Type
import qualified Elm.Docs as Docs


-- FLAGS

data Flags = Flags
    { input :: FilePath
    , output :: Maybe FilePath
    }
    deriving (Data,Typeable,Show,Eq)


defaultFlags :: Flags
defaultFlags = Flags
  { input = def &= args &= typ "FILE"

  , output = Nothing &= typFile
      &= help "file name for generated JSON documentation"

  } &= help "Generate documentation for Elm"
    &= summary "Generate documentation for Elm, (c) Evan Czaplicki"


-- GENERATE DOCUMENTATION

main :: IO ()
main =
  do  flags <- cmdArgs defaultFlags
      source <- readFile (input flags)
      case Parse.iParse documentation source of
        Right docs ->
            let json = Json.encodePretty' config docs in
            case output flags of
              Nothing -> BS.putStrLn json
              Just docPath ->
                do  createDirectoryIfMissing True (dropFileName docPath)
                    BS.writeFile docPath json

        Left err ->
          do  hPutStrLn stderr (show err)
              exitFailure


config :: Json.Config
config =
    Json.Config
    { Json.confIndent = 2
    , Json.confCompare = Json.keyOrder keys
    }
  where
    keys =
        [ "tag", "name", "comment", "aliases", "types"
        , "values", "func", "args", "type", "cases"
        ]


-- PARSE DOCUMENTATION

documentation :: Parse.IParser Docs.Documentation
documentation =
  do  optional Parse.freshLine
      (names, exports) <- Parse.header

      manyTill (string " " <|> Parse.newline <?> "more whitespace")
               (lookAhead (string "{-|") <?> "module documentation comment")

      overview <- docComment

      decls <- allDeclarations

      let (aliases, unions, values) = categorizeDeclarations decls

      return $
          Docs.Documentation
              (Module.Name names)
              overview
              (filterAliases exports aliases)
              (filterUnions exports unions)
              (filterValues exports values)


docComment :: Parse.IParser String
docComment =
  do  try (string "{-|")
      contents <- Parse.closeComment

      let reversed =
              dropWhile (`elem` " \n\r") . drop 2 $ reverse contents

      return $ dropWhile (==' ') (reverse reversed)


allDeclarations :: Parse.IParser [(String, Decl.SourceDecl)]
allDeclarations =
    Parse.onFreshLines (:) [] declaration


declaration :: Parse.IParser (String, Decl.SourceDecl)
declaration =
    uncommentable <|> commented <|> uncommented ""
  where
    uncommentable =
        (,) "" <$> Parse.infixDecl

    commented =
      do  comment <- docComment
          Parse.freshLine
          uncommented comment

    uncommented comment =
        (,) comment
            <$> choice [ Parse.typeDecl, Decl.Definition <$> Parse.typeAnnotation ]


-- CATEGORIZE DECLARATIONS

data CategoryInfo = CategoryInfo
    { aliases :: [Docs.Alias]
    , unions :: [Docs.Union]
    , values :: Map.Map String Docs.Value
    , infixes :: Map.Map String (String, Int)
    }


emptyInfo :: CategoryInfo
emptyInfo =
    CategoryInfo [] [] Map.empty Map.empty


categorizeDeclarations :: [(String, Decl.SourceDecl)] -> ([Docs.Alias], [Docs.Union], [Docs.Value])
categorizeDeclarations decls =
    (aliases, unions, Map.elems values)
  where
    (CategoryInfo aliases unions rawValues infixes) =
        foldr collectInfo emptyInfo decls

    values =
        Map.union
            (Map.intersectionWith addInfixInfo rawValues infixes)
            rawValues


addInfixInfo :: Docs.Value -> (String, Int) -> Docs.Value
addInfixInfo value infixInfo =
    value { Docs.valueAssocPrec = Just infixInfo }


collectInfo :: (String, Decl.SourceDecl) -> CategoryInfo -> CategoryInfo
collectInfo (comment, decl) info =
    case decl of
      Decl.Definition def ->
          case def of
            Source.Definition _ _ -> error errorMessage
            Source.TypeAnnotation name tipe ->
                let value = Docs.Value name comment (Type.fromInternalType tipe) Nothing
                in
                    info { values = Map.insert name value (values info) }

      Decl.Datatype name args cases ->
          let cases' = map (second (map Type.fromInternalType)) cases
              union = Docs.Union name comment args cases'
          in
              info { unions = union : unions info }

      Decl.TypeAlias name args tipe ->
          let alias = Docs.Alias name comment args (Type.fromInternalType tipe)
          in
              info { aliases = alias : aliases info }

      Decl.Fixity assoc prec name ->
          let infixInfo = (Decl.assocToString assoc, prec)
          in
              info { infixes = Map.insert name infixInfo (infixes info) }

      Decl.Port _ ->
          error errorMessage


errorMessage :: String
errorMessage =
    "there appears to be a bug in this tool.\n" ++
    "Please report it to <https://github.com/elm-lang/Elm/issues>"


-- FILTER OUT UNEXPORTED VALUES, ALIASES, and UNIONS

filterValues :: Var.Listing Var.Value -> [Docs.Value] -> [Docs.Value]
filterValues (Var.Listing exports open) values =
  case open of
    True -> values
    False ->
      let names =
            Set.fromList (Var.getValues exports)
      in
          filter (\value -> Set.member (Docs.valueName value) names) values


filterAliases :: Var.Listing Var.Value -> [Docs.Alias] -> [Docs.Alias]
filterAliases (Var.Listing exports open) aliases =
  case open of
    True -> aliases
    False ->
      let names =
            Set.fromList (Var.getAliases exports)
      in
          filter (\value -> Set.member (Docs.aliasName value) names) aliases


filterUnions :: Var.Listing Var.Value -> [Docs.Union] -> [Docs.Union]
filterUnions (Var.Listing exports open) unions =
  case open of
    True -> unions
    False ->
        Maybe.mapMaybe (filterUnion exportedUnions) unions
      where
        exportedUnions =
            Map.fromList (Var.getUnions exports)


filterUnion :: Map.Map String (Var.Listing String) -> Docs.Union -> Maybe Docs.Union
filterUnion exportedUnions union =
  case Map.lookup (Docs.unionName union) exportedUnions of
    Nothing ->
        Nothing

    Just (Var.Listing tags unionOpen)
      | unionOpen ->
          Just union
      | otherwise ->
          let publicTags =
                filter (\(tag, _) -> tag `elem` tags) (Docs.unionCases union)
          in
              Just (union { Docs.unionCases = publicTags })

