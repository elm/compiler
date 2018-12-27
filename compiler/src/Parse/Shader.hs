{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Parse.Shader
  ( shader
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified AST.Source as Src
import qualified AST.Utils.Shader as Shader
import Parse.Utils (Parser)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- SHADER


shader :: A.Position -> Parser Src.Expr
shader start@(A.Position row col) =
  do  block <- parseBlock
      shdr <- parseGlsl row col block
      end <- P.getPosition
      let src = List.intercalate "\\n" (lines block)
      return (A.at start end (Src.Shader (Utf8.fromChars src) shdr))



-- BLOCK


parseBlock :: Parser String
parseBlock =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr eerr ->
    let
      !pos6 = plusPtr pos 6
    in
    if pos6 <= end
      && P.unsafeIndex (        pos  ) == 0x5B {- [ -}
      && P.unsafeIndex (plusPtr pos 1) == 0x67 {- g -}
      && P.unsafeIndex (plusPtr pos 2) == 0x6C {- l -}
      && P.unsafeIndex (plusPtr pos 3) == 0x73 {- s -}
      && P.unsafeIndex (plusPtr pos 4) == 0x6C {- l -}
      && P.unsafeIndex (plusPtr pos 5) == 0x7C {- | -}
    then
      let
        (# status, newPos, newRow, newCol #) =
          eatShader pos6 end row col
      in
      case status of
        Good ->
          let
            !size = minusPtr newPos pos
            !block = error "TODO get a GLSL block" size
            !newState = P.State (plusPtr newPos 2) end indent newRow newCol ctx
          in
          cok block newState

        Unending ->
          cerr row col ctx E.ShaderEnd

    else
      eerr row col ctx E.ShaderStart


data Status
  = Good
  | Unending


eatShader :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Status, Ptr Word8, Word16, Word16 #)
eatShader pos end row col =
  if pos >= end then
    (# Unending, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x007C {- | -} && P.isWord (plusPtr pos 1) end 0x5D {- ] -} then
      (# Good, pos, row, col + 2 #)

    else if word == 0x0A {- \n -} then
      eatShader (plusPtr pos 1) end (row + 1) 1

    else
      let !newPos = plusPtr pos (P.getCharWidth pos end word) in
      eatShader newPos end row (col + 1)



-- GLSL


parseGlsl :: Word16 -> Word16 -> String -> Parser Shader.Types
parseGlsl startRow startCol src =
  case GLP.parse src of
    Right (GLS.TranslationUnit decls) ->
      return (foldr addInput emptyTypes (concatMap extractInputs decls))

    Left err ->
      let
        pos = Parsec.errorPos err
        row = fromIntegral (Parsec.sourceLine pos)
        col = fromIntegral (Parsec.sourceColumn pos)
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
          failure startRow (startCol + 6 + col) msg
        else
          failure (startRow + row - 1) col msg


failure :: Word16 -> Word16 -> String -> Parser a
failure row col msg =
  P.Parser $ \(P.State _ _ _ _ _ ctx) _ _ cerr _ ->
    cerr row col ctx (E.ShaderValid msg)



-- INPUTS


emptyTypes :: Shader.Types
emptyTypes =
  Shader.Types Map.empty Map.empty Map.empty


addInput :: (GLS.StorageQualifier, Shader.Type, String) -> Shader.Types -> Shader.Types
addInput (qual, tipe, name) glDecls =
  case qual of
    GLS.Attribute -> glDecls { Shader._attribute = Map.insert (Name.fromChars name) tipe (Shader._attribute glDecls) }
    GLS.Uniform   -> glDecls { Shader._uniform = Map.insert (Name.fromChars name) tipe (Shader._uniform glDecls) }
    GLS.Varying   -> glDecls { Shader._varying = Map.insert (Name.fromChars name) tipe (Shader._varying glDecls) }
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


