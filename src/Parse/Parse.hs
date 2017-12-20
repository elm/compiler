{-# OPTIONS_GHC -Wall #-}
module Parse.Parse
  ( program
  )
  where


import qualified Data.ByteString as B

import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Elm.Package as Pkg
import qualified Parse.Declaration as Decl
import qualified Parse.Module as Module
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Result as Result
import qualified Validate



-- PROGRAM


program :: Pkg.Name -> B.ByteString -> Result.Result i w Error.Error Valid.Module
program pkg src =
  let
    parser =
      if Pkg.isKernel pkg then fancyParser else normalParser
  in
  case P.run parser src of
    Right modul ->
      Validate.validate modul

    Left syntaxError ->
      Result.throw syntaxError



-- CHOMP PROGRAMS


normalParser :: P.Parser (Src.Module [Src.Decl])
normalParser =
  do  srcModule <- Module.module_ (chompDeclarations [])
      P.endOfFile
      return srcModule


fancyParser :: P.Parser (Src.Module [Src.Decl])
fancyParser =
  do  srcModule <- Module.module_ (chompDeclarations =<< chompInfixes [])
      P.endOfFile
      return srcModule



-- CHOMP DECLARATIONS


chompDeclarations :: [Src.Decl] -> P.Parser [Src.Decl]
chompDeclarations decls =
  do  (decl, _, pos) <- Decl.declaration
      P.oneOf
        [ do  P.checkFreshLine pos
              chompDeclarations (decl:decls)
        , return (reverse (decl:decls))
        ]


chompInfixes :: [Src.Decl] -> P.Parser [Src.Decl]
chompInfixes decls =
  P.oneOf
    [ do  decl <- Decl.infix_
          chompInfixes (decl:decls)
    , return decls
    ]
