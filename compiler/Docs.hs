
module Main where

import SourceSyntax.Declaration (Declaration(Datatype))
import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Types.Types ((==>), Type ( ADT, VarT ) )
import Parse.Type (datatype)
import Parse.Helpers
import Parse.Module (moduleDef)
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
  info <- many (try (annotation exports) <|> try skip <|> end)
  return (intercalate "." names, concat info)
    where
      skip = manyTill anyChar simpleNewline >> return []
      end  = many1 anyChar >> return []

annotation :: [String] -> IParser [(String, String, String)]
annotation exports =
    do description <- comment
       extras  <- option [] (lookAhead adt)
       varName <- try typeDef <|> name
       varType <- tipe
       return $ extras ++ export varName varType description
  where
    comment = concatMap clip <$> many lineComment
    clip str = case str of { ' ':rest -> rest ; _ -> str } ++ "\n"

    name = do v <- lowVar <|> parens symOp
              whitespace ; hasType ; whitespace ; return v

    tipe = manyTill anyChar (try (simpleNewline >> notFollowedBy (string " ")))

    export name tipe desc =
        if null exports || name `elem` exports then [(name,tipe,desc)] else []

    typeDef = lookAhead ((string "data" <|> string "type") >> whitespace >> capVar)

    adt = do
      (Datatype name tvs constructors) <- datatype
      let toType (cName, typeArgs) =
              (cName, show $ foldr (==>) (ADT name $ map VarT tvs) typeArgs, "")
      return $ map toType constructors

setupParser p source =
    case iParse p "" source of
      Right result -> Right result
      Left err -> Left $ "Parse error at " ++ show err
