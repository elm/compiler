module Parse.Parse (program) where

import Control.Applicative ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Parsec hiding (newline, spaces)
import qualified Text.PrettyPrint as P

import qualified AST.Declaration as D
import qualified AST.Module as M
import Parse.Helpers
import Parse.Declaration (infixDecl)
import qualified Parse.Module as Module
import qualified Parse.Declaration as Decl
import Transform.Declaration (combineAnnotations)

freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             Decl.declaration <?> "another datatype or variable definition"

decls = do d <- Decl.declaration <?> "at least one datatype or variable definition"
           (d:) <$> many freshDef

program :: OpTable -> String -> Either [P.Doc] M.ValidModule
program table src =
    do (M.Module names filePath exs ims sourceDecls) <-
           setupParserWithTable table programParser src

       decls <-
           either (\err -> Left [P.text err]) Right (combineAnnotations sourceDecls)
       return $ M.Module names filePath exs ims decls

programParser :: IParser M.SourceModule
programParser =
  do (M.HeaderAndImports names exports imports) <- Module.headerAndImports
     declarations <- decls
     optional freshLine ; optional spaces ; eof
     return $ M.Module names "" exports imports declarations

setupParserWithTable :: OpTable -> IParser a -> String -> Either [P.Doc] a
setupParserWithTable table p source =
    do localTable <- setupParser parseFixities source
       case Map.intersection table localTable of
         overlap | not (Map.null overlap) -> Left [ msg overlap ]
                 | otherwise -> 
                     flip setupParser source $ do
                       putState (Map.union table localTable)
                       p
    where
      msg overlap =
          P.vcat [ P.text "Parse error:"
                 , P.text $ "Overlapping definitions for infix operators: " ++
                          List.intercalate " " (Map.keys overlap)
                 ]

parseFixities = do
  decls <- onFreshLines (:) [] infixDecl
  return $ Map.fromList [ (op,(lvl,assoc)) | D.Fixity assoc lvl op <- decls ]
              
setupParser :: IParser a -> String -> Either [P.Doc] a
setupParser p source =
    case iParse p source of
      Right result -> Right result
      Left err -> Left [ P.text $ "Parse error at " ++ show err ]
