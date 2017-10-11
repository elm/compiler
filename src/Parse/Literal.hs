{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Literal (literal, shader) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified AST.Expression.Source as Src
import qualified AST.Literal as L
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Parse.Primitives (Parser, getPosition, oneOf)
import qualified Parse.Primitives.Number as Number
import qualified Parse.Primitives.Shader as Shader
import qualified Parse.Primitives.Utf8 as Utf8



-- LITERALS


literal :: Parser L.Literal
literal =
  oneOf
    [ L.Str <$> Utf8.string
    , L.Chr <$> Utf8.character
    , Number.number
    ]



-- SHADERS


shader :: R.Position -> Parser Src.RawExpr
shader start@(R.Position row col) =
  do  src <- Shader.block
      shdr <- parseSource row col (Text.unpack src)
      end@(R.Position row2 col2) <- getPosition
      let uid = List.intercalate ":" (map show [row, col, row2, col2])
      return (A.at start end (Src.GLShader (Text.pack uid) src shdr))


parseSource :: Int -> Int -> String -> Parser L.Shader
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


emptyShader :: L.Shader
emptyShader =
  L.Shader Map.empty Map.empty Map.empty


addInput :: (GLS.StorageQualifier, L.GLType, Text.Text) -> L.Shader -> L.Shader
addInput ( qual, tipe, name ) glDecls =
  case qual of
    GLS.Attribute ->
      glDecls { L.attribute = Map.insert name tipe (L.attribute glDecls) }

    GLS.Uniform ->
      glDecls { L.uniform = Map.insert name tipe (L.uniform glDecls) }

    GLS.Varying ->
      glDecls { L.varying = Map.insert name tipe (L.varying glDecls) }

    _ -> error "Should never happen due to below filter"


extractInputs :: GLS.ExternalDeclaration -> [(GLS.StorageQualifier, L.GLType, Text.Text)]
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
                GLS.Vec2 -> [(qual, L.V2, Text.pack name)]
                GLS.Vec3 -> [(qual, L.V3, Text.pack name)]
                GLS.Vec4 -> [(qual, L.V4, Text.pack name)]
                GLS.Mat4 -> [(qual, L.M4, Text.pack name)]
                GLS.Int -> [(qual, L.Int, Text.pack name)]
                GLS.Float -> [(qual, L.Float, Text.pack name)]
                GLS.Sampler2D -> [(qual, L.Texture, Text.pack name)]
                _ -> []
    _ -> []
