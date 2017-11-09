{-# OPTIONS_GHC -Wall #-}
module Parse.Parse
  ( program
  )
  where


import qualified Data.ByteString as B

import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Parse.Declaration as Decl
import qualified Parse.Module as Module
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import qualified Validate



-- PROGRAM


program :: B.ByteString -> Validate.Result wrn Valid.Module
program src =
  case P.run chompProgram src of
    Right modul ->
      Validate.validate modul

    Left (A.A region syntaxError) ->
      Result.throw region syntaxError


chompProgram :: P.Parser (Src.Module [Src.Decl])
chompProgram =
  do  srcModule <- Module.module_ (chompDeclarations [])
      P.endOfFile
      return srcModule


chompDeclarations :: [Src.Decl] -> P.Parser [Src.Decl]
chompDeclarations decls =
  do  (decl, _, pos) <- Decl.declaration
      P.oneOf
        [ do  P.checkFreshLine pos
              chompDeclarations (decl:decls)
        , return (reverse (decl:decls))
        ]
