{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples #-}
module Parse.Shader
  ( shader
  )
  where


import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Map as Map
import qualified Data.Name as Name
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Int (Int(..))
import GHC.Prim
import GHC.Word (Word64(..))
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import qualified AST.Source as Src
import qualified AST.Utils.Shader as Shader
import Parse.Primitives (Parser, Cursor)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- SHADER


shader :: A.Position -> Parser E.Expr Src.Expr
shader start@(A.Position cur) =
  do  block <- parseBlock
      shdr <- parseGlsl cur block
      end <- P.getPosition
      return (A.at start end (Src.Shader (Shader.fromChars block) shdr))



-- BLOCK


parseBlock :: Parser E.Expr [Char]
parseBlock =
  P.Parser $ \fpc (P.State pos end indent cur) cok _ cerr eerr ->
    let
      !pos6 = plusAddr# pos 6#
    in
    if P.leAddr pos6 end
      && P.eqIndex pos 0# 0x5B#Word8 {- [ -}
      && P.eqIndex pos 1# 0x67#Word8 {- g -}
      && P.eqIndex pos 2# 0x6C#Word8 {- l -}
      && P.eqIndex pos 3# 0x73#Word8 {- s -}
      && P.eqIndex pos 4# 0x6C#Word8 {- l -}
      && P.eqIndex pos 5# 0x7C#Word8 {- | -}
    then
      let
        !(# status, newPos, newCur #) =
          eatShader pos6 end (P.slide cur 6#Word64)
      in
      case status of
        Good ->
          let
            !block = BS_UTF8.toString (BS.BS (ForeignPtr pos fpc) (I# (minusAddr# end pos)))
            !newState = P.State (plusAddr# newPos 2#) end indent (P.slide newCur 2#Word64)
          in
          cok block newState

        Unending -> cerr cur E.ShaderEndless
        NotUtf8  -> cerr cur E.ShaderNotUtf8

    else
      eerr cur E.Start


data Status
  = Good
  | Unending
  | NotUtf8


eatShader :: Addr# -> Addr# -> Cursor -> (# Status, Addr#, Cursor #)
eatShader pos end cur =
  if P.notLtAddr pos end then
    (# Unending, pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x7C#Word8 {-|-} | P.ltAddr (plusAddr# pos 1#) end && P.eqIndex pos 1# 0x5D#Word8 {-]-} ->
        (# Good, pos, cur #)

      0x0A#Word8 {- \n -} ->
        eatShader (plusAddr# pos 1#) end (P.newline cur)

      word ->
        let !newPos = P.skipUtf8 pos end word in
        if P.eqAddr pos newPos
          then (# NotUtf8, pos, cur #)
          else eatShader newPos end (P.slide cur 1#Word64)



-- GLSL


parseGlsl :: Cursor -> [Char] -> Parser E.Expr Shader.Types
parseGlsl cur src =
  case GLP.parse src of
    Right (GLS.TranslationUnit decls) ->
      return (foldr addInput emptyTypes (concatMap extractInputs decls))

    Left err ->
      P.Parser $ \_ _ _ _ cerr _ ->
        let
          pos = Parsec.errorPos err
          row = fromIntegral (Parsec.sourceLine   pos - 1)
          col = fromIntegral (Parsec.sourceColumn pos - 1)
          msg =
            Parsec.showErrorMessages
              "or"
              "unknown parse error"
              "expecting"
              "unexpected"
              "end of input"
              (Parsec.errorMessages err)
        in
        cerr (jump (P.slide cur 6#Word64) row col) (E.ShaderProblem msg)


jump :: Cursor -> Word64 -> Word64 -> Cursor
jump cur (W64# row) (W64# col) =
  case row of
    0#Word64 -> P.slide cur col
    _        -> P.slide (and64# (plusWord64# cur (uncheckedShiftL64# row 32#)) 0xFFFFFFFF00000000#Word64) col



-- INPUTS


emptyTypes :: Shader.Types
emptyTypes =
  Shader.Types Map.empty Map.empty Map.empty


addInput :: (GLS.StorageQualifier, Shader.Type, [Char]) -> Shader.Types -> Shader.Types
addInput (qual, tipe, name) glDecls =
  case qual of
    GLS.Attribute -> glDecls { Shader._attribute = Map.insert (Name.fromChars name) tipe (Shader._attribute glDecls) }
    GLS.Uniform   -> glDecls { Shader._uniform = Map.insert (Name.fromChars name) tipe (Shader._uniform glDecls) }
    GLS.Varying   -> glDecls { Shader._varying = Map.insert (Name.fromChars name) tipe (Shader._varying glDecls) }
    _             -> error "Should never happen due to `extractInputs` function"


extractInputs :: GLS.ExternalDeclaration -> [(GLS.StorageQualifier, Shader.Type, [Char])]
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


