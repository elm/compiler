{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Literal (literal) where

--import Prelude hiding (exponent)
--import Control.Monad (join)
--import qualified Data.Char as Char
--import qualified Data.List as List
--import qualified Data.Map as Map
--import qualified Language.GLSL.Parser as GLP
--import qualified Language.GLSL.Syntax as GLS

import qualified AST.Literal as L
import Parse.Helpers (Parser, string, character, number, oneOf)
--import qualified Reporting.Error.Syntax as Error



-- LITERALS


literal :: Parser L.Literal
literal =
  oneOf
    [ L.Str <$> string
    , L.Chr <$> character
    , number
    ]



{-- SHADERS


shader :: Parser (String, L.GLShaderTipe)
shader =
  do  text "[glsl|"
      rawSrc <- closeShader id
      case stringToShader rawSrc of
        Left err -> parserFail . show $ err
        Right tipe -> return (rawSrc, tipe)


closeShader :: (String -> a) -> Parser a
closeShader builder =
  choice
    [ do  try (string "|]")
          return (builder "")
    , do  c <- anyChar
          closeShader (builder . (c:))
    ]


stringToShader :: String -> Either ParseError L.Shader
stringToShader src =
  case GLP.parse src of
    Left e ->
      Left e

    Right (GLS.TranslationUnit decls) ->
      Right $ foldr addGLinput emptyDecls $ join $
        map extractGLinputs decls
  where
    emptyDecls = L.GLShaderTipe Map.empty Map.empty Map.empty

    addGLinput (qual,tipe,name) glDecls =
      case qual of
        GLS.Attribute ->
            glDecls { L.attribute = Map.insert name tipe $ L.attribute glDecls }

        GLS.Uniform ->
            glDecls { L.uniform = Map.insert name tipe $ L.uniform glDecls }

        GLS.Varying ->
            glDecls { L.varying = Map.insert name tipe $ L.varying glDecls }

        _ -> error "Should never happen due to below filter"

    extractGLinputs decl =
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
                    GLS.Vec2 -> return (qual, L.V2, name)
                    GLS.Vec3 -> return (qual, L.V3, name)
                    GLS.Vec4 -> return (qual, L.V4, name)
                    GLS.Mat4 -> return (qual, L.M4, name)
                    GLS.Int -> return (qual, L.Int, name)
                    GLS.Float -> return (qual, L.Float, name)
                    GLS.Sampler2D -> return (qual, L.Texture, name)
                    _ -> []
        _ -> []
-}