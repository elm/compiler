{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Literal (literal, shader) where

import Prelude hiding (exponent)
import Control.Monad (join)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import Text.Parsec
  ( ParseError, (<|>), anyChar, char, choice, digit, getPosition
  , hexDigit, lookAhead, many1, option, parserFail, satisfy
  , setPosition, string, try, upper
  )

import qualified AST.Literal as L
import Parse.Helpers (IParser, expecting, failure)
import qualified Reporting.Error.Syntax as Error



-- LITERALS


literal :: IParser L.Literal
literal =
  choice
    [ toLiteral <$> rawNumber
    , stringLiteral
    , charLiteral
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



-- CHARACTERS


charLiteral :: IParser L.Literal
charLiteral =
  expecting "a character like 'c'" $
    do  start <- getPosition
        char '\''
        firstChar <-
          choice
            [ char '\\' >> escapeCode
            , satisfy (\c -> c /= '\'' && c > '\026')
            ]
        choice
          [ do  char '\''
                return (L.Chr firstChar)
          , do  setPosition start
                failure Error.singleQuotes
          ]



-- STRINGS


stringLiteral :: IParser L.Literal
stringLiteral =
  expecting "a string like \"hello\"" $
    do  char '"'
        choice
          [ do  try (string "\"\"")
                multiStringClose mempty
          , do  singleStringClose mempty
          ]


singleStringClose :: (String -> String) -> IParser L.Literal
singleStringClose builder =
  choice
    [ do  char '"'
          return (L.Str (builder ""))
    , do  char '\\'
          choice
            [ do  char '&'
                  singleStringClose builder
            , do  c <- escapeCode
                  singleStringClose (\end -> builder (c : end))
            ]
    , do  c <- satisfy (\c -> c > '\026')
          singleStringClose (\end -> builder (c : end))
    , failure "Not expecting a character like this in a string."
    ]


multiStringClose :: (String -> String) -> IParser L.Literal
multiStringClose builder =
  choice
    [ do  try (string "\"\"\"")
          return (L.Str (builder ""))
    , do  char '\n'
          multiStringClose (\end -> builder ('\n' : end))
    , do  char '\\'
          choice
            [ do  char '&'
                  multiStringClose builder
            , do  c <- escapeCode
                  multiStringClose (\end -> builder (c : end))
            ]
    , do  c <- satisfy (\c -> c > '\026')
          multiStringClose (\end -> builder (c : end))
    , failure "Not expecting a character like this in a multi-line string."
    ]


escapeCode :: IParser Char
escapeCode =
  choice
    [ do  char '^'
          code <- upper
          return (toEnum (fromEnum code - fromEnum 'A' + 1))
    -- NUMBERS
    , do  parseCode 10 digit
    , do  char 'x'
          parseCode 16 hexDigit
    -- ESCAPES
    , c2c 'a' '\a'
    , c2c 'b' '\b'
    , c2c 'f' '\f'
    , c2c 'n' '\n'
    , c2c 'r' '\r'
    , c2c 't' '\t'
    , c2c 'v' '\v'
    , c2c '"' '"'
    , c2c '\\' '\\'
    , c2c '\'' '\''
    -- ASCII CODES
    , s2c "BS" '\BS'
    , s2c "HT" '\HT'
    , s2c "LF" '\LF'
    , s2c "VT" '\VT'
    , s2c "FF" '\FF'
    , s2c "CR" '\CR'
    , s2c "SO" '\SO'
    , s2c "SI" '\SI'
    , s2c "EM" '\EM'
    , s2c "FS" '\FS'
    , s2c "GS" '\GS'
    , s2c "RS" '\RS'
    , s2c "US" '\US'
    , s2c "SP" '\SP'
    , s2c "NUL" '\NUL'
    , s2c "SOH" '\SOH'
    , s2c "STX" '\STX'
    , s2c "ETX" '\ETX'
    , s2c "EOT" '\EOT'
    , s2c "ENQ" '\ENQ'
    , s2c "ACK" '\ACK'
    , s2c "BEL" '\BEL'
    , s2c "DLE" '\DLE'
    , s2c "DC1" '\DC1'
    , s2c "DC2" '\DC2'
    , s2c "DC3" '\DC3'
    , s2c "DC4" '\DC4'
    , s2c "NAK" '\NAK'
    , s2c "SYN" '\SYN'
    , s2c "ETB" '\ETB'
    , s2c "CAN" '\CAN'
    , s2c "SUB" '\SUB'
    , s2c "ESC" '\ESC'
    , s2c "DEL" '\DEL'
    ]


c2c :: Char -> Char -> IParser Char
c2c escapedChar resultingChar =
  do  char escapedChar
      return resultingChar


s2c :: String -> Char -> IParser Char
s2c escapedString resultingChar =
  do  string escapedString
      return resultingChar


parseCode :: Integer -> IParser Char -> IParser Char
parseCode base baseDigit =
  do  start <- getPosition
      code <- parseNumber base baseDigit
      if code <= 0x10FFFF
        then
          return (toEnum (fromInteger code))

        else
          do  setPosition start
              fail "This code is too big! UTF-8 only goes to 10FFFF."


parseNumber :: Integer -> IParser Char -> IParser Integer
parseNumber base baseDigit =
  do  digits <- many1 baseDigit
      let n = List.foldl' (\x d -> base * x + toInteger (Char.digitToInt d)) 0 digits
      seq n (return n)



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
