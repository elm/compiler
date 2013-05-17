
module Main where

import Ast
import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Parse.Library
import Parse.Modules (moduleDef)
import Text.Parsec hiding (newline,spaces)
import System.Environment
import System.Exit


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
    ",\n          \"type\" : " ++ show tipe ++
    ",\n          \"desc\" : " ++ show desc ++ "\n        }"

docParse :: String -> Either String (String, [(String, String, String)])
docParse = setupParser $ do
  optional freshLine
  (names, exports) <- option (["Main"],[]) moduleDef
  info <- many (annotation exports <|> try skip <|> end)
  return (intercalate "." names, catMaybes info)
    where
      skip = manyTill anyChar simpleNewline >> return Nothing
      end  = many1 anyChar >> return Nothing

annotation :: [String] -> IParser (Maybe (String, String, String))
annotation exports =
    try ((\c n t -> export (n,t,c)) <$> comment <*> (try adt <|> name) <*> tipe)
  where
    comment = concatMap clip <$> many lineComment
    clip str = case str of { ' ':rest -> rest ; _ -> str } ++ "\n"

    name = do v <- lowVar <|> parens symOp
              whitespace ; hasType ; whitespace ; return v

    tipe = manyTill anyChar (try (simpleNewline >> notFollowedBy (string " ")))

    export info@(name,_,_) =
        if null exports || name `elem` exports then Just info else Nothing

    adt = lookAhead ((string "data" <|> string "type") >> whitespace >> capVar)


setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
