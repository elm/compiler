
module Main where

import Ast
import Control.Applicative ((<$>), (<*>))
import Data.List as List
import Parse.Library
import Parse.Modules (moduleDef)
import Parse.Types (typeAnnotation)
import System.Environment
import System.Exit
import Text.Parsec hiding (newline,spaces)
import Types.Types


main = do
  srcs <- mapM readFile =<< getArgs
  case mapM docParse srcs of
    Left err -> putStrLn err >> exitFailure
    Right ms -> putStrLn (toModules ms)

toModules ms =
  "{ \"modules\" : [\n    " ++ intercalate ",\n    " (map toModule ms) ++ "\n  ]\n}"

toModule (name, values) =
  "{ \"module\" : " ++ show name ++ ",\n      \"values\" : [\n        " ++ vs ++ "\n      ]\n    }"
    where vs = intercalate ",\n        " (map toValue values)

toValue (name, tipe) =
    "{ \"name\" : " ++ show name ++ ",\n          \"type\" : \"" ++ show tipe ++ "\"\n        }"

docParse :: String -> Either String (String, [(String, Type)])
docParse = setupParser $ do
             optional freshLine
             (,) <$> option "Main" moduleName <*> types
    where 
      skip = manyTill anyChar simpleNewline >> return []
      end  = many1 anyChar >> return []
      tipe = get <$> try typeAnnotation
      get stmt = case stmt of { TypeAnnotation n t -> [(n,t)] ; _ -> [] }
      types = concat <$> many (tipe <|> try skip <|> end)
      getName = intercalate "." . fst
      moduleName = do optional freshLine 
                      getName <$> moduleDef `followedBy` freshLine

setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
