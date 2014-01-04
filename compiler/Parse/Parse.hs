{-# OPTIONS_GHC -W #-}
module Parse.Parse (program, dependencies) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Parsec hiding (newline,spaces)
import qualified Text.PrettyPrint as P

import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Module as M
import Parse.Helpers
import Parse.Declaration (infixDecl)
import Parse.Module
import qualified Parse.Declaration as Decl
import Transform.Declaration (combineAnnotations)

freshDef = commitIf (freshLine >> (letter <|> char '_')) $ do
             freshLine
             Decl.declaration <?> "another datatype or variable definition"

decls = do d <- Decl.declaration <?> "at least one datatype or variable definition"
           (d:) <$> many freshDef

program :: OpTable -> String -> Either [P.Doc] (M.Module D.Declaration)
program table src =
    do (M.Module names exs ims parseDecls) <- setupParserWithTable table programParser src
       decls <- either (\err -> Left [P.text err]) Right (combineAnnotations parseDecls)
       return $ M.Module names exs ims decls

programParser :: IParser (M.Module D.ParseDeclaration)
programParser =
    do optional freshLine
       (names,exports) <- option (["Main"],[]) (moduleDef `followedBy` freshLine)
       is <- (do try (lookAhead $ reserved "import")
                 imports `followedBy` freshLine) <|> return []
       declarations <- decls
       optional freshLine ; optional spaces ; eof
       return $ M.Module names exports is declarations

dependencies :: String -> Either [P.Doc] (String, [String])
dependencies =
    let getName = List.intercalate "." . fst in
    setupParser $ do
      optional freshLine
      (,) <$> option "Main" (getName <$> moduleDef `followedBy` freshLine)
          <*> option [] (map fst <$> imports `followedBy` freshLine)

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
