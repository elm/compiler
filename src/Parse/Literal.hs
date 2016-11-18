{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Literal (literal, shader) where

import Prelude hiding (exponent)
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import qualified Text.Parsec.Token as T
import Text.Parsec
  ( ParseError, (<|>), (<?>), anyChar, char, choice, digit, hexDigit
  , lookAhead, many1, manyTill, notFollowedBy, option, parserFail
  , satisfy, string, try
  )

import Parse.Helpers (IParser, SourceM, expecting, failure, iParse, reserveds)
import qualified AST.Literal as L



-- LITERALS


literal :: IParser L.Literal
literal =
  choice
    [ toLiteral <$> rawNumber
    , {-# SCC elm_compiler_parse_str #-} L.Str <$> str
    , L.Chr <$> chr
    ]



-- NUMBERS


toLiteral :: String -> L.Literal
toLiteral n
  | 'x' `elem` n         = L.IntNum (read n)
  | any (`elem` ".eE") n = L.FloatNum (read n)
  | otherwise            = L.IntNum (read n)


rawNumber :: IParser String
rawNumber =
  concat <$> sequence
    [ option "" minus
    , base16 <|> base10
    ]


base16 :: IParser String
base16 =
  do  try (string "0x")
      digits <- many1 hexDigit
      return ("0x" ++ digits)


base10 :: IParser String
base10 =
  concat <$> sequence
    [ many1 digit
    , option "" decimals
    , option "" exponent
    ]


minus :: IParser String
minus =
  try $ do
    string "-"
    lookAhead digit
    return "-"


decimals :: IParser String
decimals =
  do  try $ lookAhead (string "." >> digit)
      string "."
      n <- many1 digit
      return ('.' : n)


exponent :: IParser String
exponent =
  do  string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)



-- STRINGS


str :: IParser String
str =
  expecting "a string like \"hello\"" $
  do  charSeq <- choice [ multiStr, singleStr ]
      either (fail . show) return $
        processAs T.stringLiteral ('"' : charSeq ++ "\"")


delimitedSequence :: IParser a -> IParser String -> IParser String
delimitedSequence quote insides =
  do  quote
      concat <$> manyTill insides quote


multiStr :: IParser String
multiStr =
  delimitedSequence (try (string "\"\"\"")) multilineStringChar


singleStr :: IParser String
singleStr =
  delimitedSequence (char '"') (charWithin '"')



-- CHARACTERS


chr :: IParser Char
chr =
  expecting "a character like 'x'" $
  do  char '\''
      charSeq <- concat <$> many1 (charWithin '\'') <?> "a character"
      case processAs T.charLiteral ("'" ++ charSeq ++ "'") of
        Right character ->
          do  char '\''
              return character

        Left _ ->
          failure $
            "Elm uses double quotes for strings. Switch\
            \ the ' to \" on both ends of the string and\
            \ you should be all set!"



-- CHARACTER HELPERS


charWithin :: Char -> IParser String
charWithin delim =
  choice
    [ escaped delim
    , (:[]) <$> satisfy (/= delim)
    ]


multilineStringChar :: IParser String
multilineStringChar =
  do  notFollowedBy (string "\"\"\"")
      choice
        [ newlineChar
        , escaped '\"'
        , escapeQuotes <$> anyChar
        ]


escapeQuotes :: Char -> String
escapeQuotes c =
  if c == '\"' then "\\\"" else [c]


newlineChar :: IParser String
newlineChar =
  choice
    [ char '\n' >> return "\\n"
    , char '\r' >> return "\\r"
    ]


escaped :: Char -> IParser String
escaped delim =
  try $ do
    char '\\'
    c <- char '\\' <|> char delim
    return ['\\', c]


processAs
  :: (T.GenTokenParser String u SourceM -> IParser a)
  -> String
  -> Either ParseError a
processAs processor rawString =
    iParse (processor lexer) rawString
  where
    lexer :: T.GenTokenParser String u SourceM
    lexer =
      T.makeTokenParser elmDef

    -- I don't know how many of these are necessary for charLiteral/stringLiteral
    elmDef :: T.GenLanguageDef String u SourceM
    elmDef =
      T.LanguageDef
        { T.commentStart    = "{-"
        , T.commentEnd      = "-}"
        , T.commentLine     = "--"
        , T.nestedComments  = True
        , T.identStart      = undefined
        , T.identLetter     = undefined
        , T.opStart         = undefined
        , T.opLetter        = undefined
        , T.reservedNames   = reserveds
        , T.reservedOpNames = [":", "->", "|"]
        , T.caseSensitive   = True
        }



-- SHADERS


shader :: IParser (String, L.GLShaderTipe)
shader =
  do  try (string "[glsl|")
      rawSrc <- closeShader id
      case glSource rawSrc of
        Left err -> parserFail . show $ err
        Right tipe -> return (rawSrc, tipe)


closeShader :: (String -> a) -> IParser a
closeShader builder =
  choice
    [ do  try (string "|]")
          return (builder "")
    , do  c <- anyChar
          closeShader (builder . (c:))
    ]


glSource :: String -> Either ParseError L.GLShaderTipe
glSource src =
  case GLP.parse src of
    Left e -> Left e
    Right (GLS.TranslationUnit decls) ->
      map extractGLinputs decls
        |> join
        |> foldr addGLinput emptyDecls
        |> Right
  where
    (|>) = flip ($)

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
                    GLS.Int -> return (qual, L.Int,name)
                    GLS.Float -> return (qual, L.Float,name)
                    GLS.Vec2 -> return (qual, L.V2,name)
                    GLS.Vec3 -> return (qual, L.V3,name)
                    GLS.Vec4 -> return (qual, L.V4,name)
                    GLS.Mat4 -> return (qual, L.M4,name)
                    GLS.Sampler2D -> return (qual, L.Texture,name)
                    _ -> []
        _ -> []
