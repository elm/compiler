{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Shader
  ( shader
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified AST.Source as Src
import qualified AST.Utils.Shader as Shader
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Parse.Primitives (Parser, getPosition)
import qualified Parse.Primitives.Shader as Shader



-- SHADERS


shader :: R.Position -> Parser Src.Expr
shader start@(R.Position row col) =
  do  block <- Shader.block
      shdr <- parseSource row col (Text.unpack block)
      end@(R.Position row2 col2) <- getPosition
      let uid = List.intercalate ":" (map show [row, col, row2, col2])
      let src = Text.replace "\n" "\\n" (Text.replace "\r\n" "\\n" block)
      return (A.at start end (Src.Shader (Text.pack uid) src shdr))


parseSource :: Int -> Int -> String -> Parser Shader.Shader
parseSource startRow startCol src =
  case GLP.parse src of
    Right (GLS.TranslationUnit decls) ->
      return (foldr addInput emptyShader (concatMap extractInputs decls))

    Left err ->
      let
        pos = Parsec.errorPos err
        row = Parsec.sourceLine pos
        col = Parsec.sourceColumn pos
        msg =
          Parsec.showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (Parsec.errorMessages err)
      in
        if row == 1 then
          Shader.failure startRow (startCol + 6 + col) (Text.pack msg)
        else
          Shader.failure (startRow + row - 1) col (Text.pack msg)


emptyShader :: Shader.Shader
emptyShader =
  Shader.Shader Map.empty Map.empty Map.empty


addInput :: (GLS.StorageQualifier, Shader.Type, String) -> Shader.Shader -> Shader.Shader
addInput (qual, tipe, name) glDecls =
  case qual of
    GLS.Attribute -> glDecls { Shader._attribute = Map.insert (N.fromString name) tipe (Shader._attribute glDecls) }
    GLS.Uniform   -> glDecls { Shader._uniform = Map.insert (N.fromString name) tipe (Shader._uniform glDecls) }
    GLS.Varying   -> glDecls { Shader._varying = Map.insert (N.fromString name) tipe (Shader._varying glDecls) }
    _             -> error "Should never happen due to `extractInputs` function"


extractInputs :: GLS.ExternalDeclaration -> [(GLS.StorageQualifier, Shader.Type, String)]
extractInputs decl =
  case decl of
    GLS.Declaration
      (GLS.InitDeclaration
         (GLS.TypeDeclarator
            (GLS.FullType
               (Just (GLS.TypeQualSto qual))
               (GLS.TypeSpec _prec (GLS.TypeSpecNoPrecision tipe _mexpr1))))
         [GLS.InitDecl name _mexpr2 _mexpr3]
      ) ->
        case elem qual [GLS.Attribute, GLS.Varying, GLS.Uniform] of
          False -> []
          True ->
              case tipe of
                GLS.Vec2 -> [(qual, Shader.V2, name)]
                GLS.Vec3 -> [(qual, Shader.V3, name)]
                GLS.Vec4 -> [(qual, Shader.V4, name)]
                GLS.Mat4 -> [(qual, Shader.M4, name)]
                GLS.Int -> [(qual, Shader.Int, name)]
                GLS.Float -> [(qual, Shader.Float, name)]
                GLS.Sampler2D -> [(qual, Shader.Texture, name)]
                _ -> []
    _ -> []
