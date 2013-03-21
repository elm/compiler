
module Main where

import Ast
import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Parse.Library
import Parse.Modules (moduleDef)
import Parse.Types (typeAnnotation)
import Text.Parsec hiding (newline,spaces)
import System.Environment
import System.Exit
import Types.Types


main = do
  srcs <- mapM readFile =<< getArgs
  case mapM docParse srcs of
    Left err -> putStrLn err >> exitFailure
    Right ms -> putStrLn (toModules ms)

toModules ms = wrap (intercalate ",\n    " (map toModule ms))
  where wrap s = "{ \"modules\" : [\n    " ++ s ++ "\n  ]\n}"

toModule (name, values) =
    "{ \"name\" : " ++ show name ++ ",\n      " ++
    "\"values\" : [\n        " ++ vs ++ "\n      ]\n    }"
  where vs = intercalate ",\n        " (map toValue values)

toValue (name, tipe, desc) =
    "{ \"name\" : " ++ show name ++
    ",\n          \"type\" : \"" ++ show tipe ++
    "\",\n          \"desc\" : " ++ show desc ++ "\n        }"

docParse :: String -> Either String (String, [(String, Type, String)])
docParse = setupParser $ do
             optional freshLine
             (,) <$> option "Main" moduleName <*> types
    where
      skip = manyTill anyChar simpleNewline >> return []
      end  = many1 anyChar >> return []
      types = concat <$> many (docs <|> try skip <|> end)
      getName = intercalate "." . fst
      moduleName = do optional freshLine
                      getName <$> moduleDef `followedBy` freshLine

docs :: IParser [(String, Type, String)]
docs = (tipe <$> try typeAnnotation) <|> commentTipe
  where
    clip str = case str of { ' ':rest -> rest ; _ -> str }
    tipe stmt = case stmt of { TypeAnnotation n t -> [(n,t,"")] ; _ -> [] }
    commentTipe = do
      cs  <- map clip <$> many1 lineComment
      typ <- optionMaybe (try typeAnnotation)
      return $ case typ of
                 Just (TypeAnnotation n t) -> [(n, t, intercalate "\n" cs)]
                 _ -> []


setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
