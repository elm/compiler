{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Parse (program, parse) where

import qualified Data.Map as Map
import qualified Data.Traversable as T
import Text.Parsec (char, eof, letter, many, putState, (<|>))
import qualified Text.Parsec.Error as Parsec

import qualified AST.Declaration as Decl
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Package
import Parse.Helpers
import qualified Parse.Module as Parse (header)
import qualified Parse.Declaration as Parse (declaration, infixDecl)
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import qualified Validate



program
    :: Package.Name
    -> OpTable
    -> String
    -> Result.Result wrn Error.Error Module.Valid
program pkgName table src =
  do  modul <-
        parseWithTable table src (programParser pkgName)

      let
        (Module.Source docs exports imports sourceDecls) =
          Module.info modul

      validDecls <-
        Validate.declarations sourceDecls

      return $
        modul
          { Module.info =
              Module.Valid
                docs
                exports
                (addDefaults pkgName imports)
                validDecls
                (error "TODO")
                (error "TODO")
          }


-- determine if default imports should be added, only elm-lang/core is exempt
addDefaults
  :: Package.Name
  -> [Module.UserImport]
  -> ([Module.DefaultImport], [Module.UserImport])
addDefaults pkgName imports =
  flip (,) imports $
    if pkgName == Package.coreName then
      []

    else
      Imports.defaults



-- HEADERS AND DECLARATIONS


programParser :: Package.Name -> IParser Module.Source
programParser pkgName =
  do  (Module.Header tag name docs exports imports) <-
        Parse.header

      decls <-
        declarations

      many (spaces <|> newline)

      eof

      return $
        Module.Module
          tag
          (ModuleName.Canonical pkgName name)
          ""
          (Module.Source docs exports imports decls)


declarations :: IParser [Decl.Source]
declarations =
  (:) <$> Parse.declaration
      <*> many freshDef


freshDef :: IParser Decl.Source
freshDef =
    commitIf (freshLine >> (letter <|> char '_')) $
      do  freshLine
          Parse.declaration



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
    :: Map.Map String (Int, Decl.Assoc)
    -> [(String, InfixInfo)]
    -> Result.Result wrn Error.Error (Map.Map String (Int, Decl.Assoc))
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
          (Decl.Fixity assoc level op) <- Parse.infixDecl
          end <- getMyPosition
          return (op, InfixInfo (R.Region start end) (level, assoc))


data InfixInfo = InfixInfo
    { _region :: R.Region
    , _info :: (Int, Decl.Assoc)
    }
