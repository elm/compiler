{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Parse (program, parse) where

import qualified Data.Map as Map
import qualified Data.Traversable as T
import Text.Parsec (char, eof, letter, many, putState, (<|>))
import qualified Text.Parsec.Error as Parsec

import qualified AST.Declaration as Decl
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Package
import Parse.Helpers
import qualified Parse.Module as Parse (header)
import qualified Parse.Declaration as Parse (declaration, infixDecl)
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import qualified Validate



program :: Package.Name -> OpTable -> String -> Validate.Result wrn Module.Valid
program pkgName table src =
  do  modul <-
        parseWithTable table src (programParser pkgName)

      Validate.module' modul



-- HEADERS AND DECLARATIONS


programParser :: Package.Name -> IParser Module.Source
programParser pkgName =
  do  (Module.Header tag name exports settings docs imports) <-
        Parse.header

      decls <-
        declarations

      many (spaces <|> newline)

      eof

      return $
        Module.Module
          (ModuleName.Canonical pkgName name)
          ""
          (Module.Source tag settings docs exports imports decls)


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


parse :: String -> IParser a -> Validate.Result wrn a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let pos = R.fromSourcePos (Parsec.errorPos err)
            msgs = Parsec.errorMessages err
        in
            Result.throw (R.Region pos pos) (Error.Parse msgs)


parseWithTable :: OpTable -> String -> IParser a -> Validate.Result wrn a
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
    -> Validate.Result wrn (Map.Map String (Int, Decl.Assoc))
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
          (Decl.Fixity (Decl.Infix op assoc level)) <- Parse.infixDecl
          end <- getMyPosition
          return (op, InfixInfo (R.Region start end) (level, assoc))


data InfixInfo = InfixInfo
    { _region :: R.Region
    , _info :: (Int, Decl.Assoc)
    }
