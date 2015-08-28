{-# OPTIONS_GHC -Wall #-}
module Parse.Parse (program, parse) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as Map
import qualified Data.Traversable as T
import Text.Parsec (char, eof, letter, many, optional, putState, (<|>))
import qualified Text.Parsec.Error as Parsec

import qualified AST.Declaration as D
import qualified AST.Module as M
import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Package
import Parse.Helpers
import qualified Parse.Module as Module
import qualified Parse.Declaration as Decl
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import qualified Validate


program
    :: Package.Name
    -> Bool
    -> OpTable
    -> String
    -> Result.Result wrn Error.Error M.ValidModule
program pkgName isRoot table src =
  do  (M.Module name filePath docs exports imports sourceDecls) <-
          parseWithTable table src (programParser pkgName)

      validDecls <- Validate.declarations isRoot sourceDecls

      -- determine if default imports should be added, only elm-lang/core is exempt
      let ammendedImports =
            ( if pkgName == Package.coreName then [] else Imports.defaults
            , imports
            )

      return (M.Module name filePath docs exports ammendedImports validDecls)


-- HEADERS AND DECLARATIONS

programParser :: Package.Name -> IParser M.SourceModule
programParser pkgName =
  do  (M.Header name docs exports imports) <- Module.header
      decls <- declarations
      optional freshLine
      optional spaces
      eof

      let canonicalName =
            ModuleName.Canonical pkgName name

      return $ M.Module canonicalName "" docs exports imports decls


declarations :: IParser [D.SourceDecl]
declarations =
  (:) <$> Decl.declaration
      <*> many freshDef


freshDef :: IParser D.SourceDecl
freshDef =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  _ <- freshLine
          Decl.declaration


-- RUN PARSERS

parse :: String -> IParser a -> Result.Result wrn Error.Error a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let pos = R.fromSourcePos (Parsec.errorPos err)
            msgs = Parsec.errorMessages err
        in
            Result.throw (R.Region pos pos) (Error.Parse msgs)


parseWithTable
    :: OpTable
    -> String
    -> IParser a
    -> Result.Result wrn Error.Error a
parseWithTable table source parser =
  do  infixInfoList <- parse source parseFixities

      infixTable <- makeInfixTable table infixInfoList

      parse source $
          do  putState infixTable
              parser



-- INFIX INFO

makeInfixTable
    :: Map.Map String (Int, D.Assoc)
    -> [(String, InfixInfo)]
    -> Result.Result wrn Error.Error (Map.Map String (Int, D.Assoc))
makeInfixTable table newInfo =
  let add (op, info) dict =
        Map.insertWith (++) op [info] dict

      infoTable =
        foldr add Map.empty newInfo

      check op infoList =
        case infoList of
          [] ->
              error "problem parsing infix declarations, this should never happen"

          [InfixInfo region info] ->
              if Map.member op table
                then Result.throw region (Error.InfixDuplicate op)
                else return info

          InfixInfo region _ : _ ->
              Result.throw region (Error.InfixDuplicate op)
  in
      Map.union table <$> T.sequenceA (Map.mapWithKey check infoTable)


parseFixities :: IParser [(String, InfixInfo)]
parseFixities =
    onFreshLines (:) [] infics
  where
    infics =
      do  start <- getMyPosition
          (D.Fixity assoc level op) <- Decl.infixDecl
          end <- getMyPosition
          return (op, InfixInfo (R.Region start end) (level, assoc))


data InfixInfo = InfixInfo
    { _region :: R.Region
    , _info :: (Int, D.Assoc)
    }