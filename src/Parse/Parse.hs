{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Parse (program) where

import Text.Parsec (char, eof, letter, many, (<|>))
import qualified Text.Parsec.Error as Parsec

import qualified AST.Declaration as Decl
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Package
import Parse.Helpers (IParser, commitIf, freshLine, newline, parse, spaces)
import qualified Parse.Module as Parse (header)
import qualified Parse.Declaration as Parse (declaration)
import qualified Reporting.Region as R
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import qualified Validate



program :: Package.Name -> String -> Validate.Result wrn Module.Valid
program pkgName src =
  do  modul <- toResult (parse (programParser pkgName) src)
      Validate.module' modul


-- HEADERS AND DECLARATIONS


programParser :: Package.Name -> IParser Module.Source
programParser pkgName =
  do  (Module.Header tag name exports settings docs imports) <- Parse.header
      decls <- declarations
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



-- PARSER OUTPUT TO RESULT


toResult :: Either Parsec.ParseError a -> Validate.Result wrn a
toResult parserOutput =
  case parserOutput of
    Right result ->
        return result

    Left err ->
        let pos = R.fromSourcePos (Parsec.errorPos err)
            msgs = Parsec.errorMessages err
        in
            Result.throw (R.Region pos pos) (Error.Parse msgs)