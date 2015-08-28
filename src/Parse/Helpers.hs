module Parse.Helpers where

import Prelude hiding (until)
import Control.Applicative ((<$>),(<*>),(<*))
import Control.Monad (guard, join)
import Control.Monad.State (State)
import Data.Char (isUpper)
import qualified Data.Map as Map
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import Text.Parsec hiding (newline, spaces, State)
import Text.Parsec.Indent (indented, runIndent)
import qualified Text.Parsec.Token as T

import qualified AST.Declaration as Decl
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Variable as Variable
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Region as R


reserveds :: [String]
reserveds =
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "as", "hiding", "exposing"
    , "port", "export", "foreign"
    , "perform"
    , "deriving"
    ]


-- ERROR HELP

expecting = flip (<?>)


-- SETUP

type OpTable = Map.Map String (Int, Decl.Assoc)
type SourceM = State SourcePos
type IParser a = ParsecT String OpTable SourceM a


iParse :: IParser a -> String -> Either ParseError a
iParse parser source =
  iParseWithTable "" Map.empty parser source


iParseWithTable :: SourceName -> OpTable -> IParser a -> String -> Either ParseError a
iParseWithTable sourceName table aParser input =
  runIndent sourceName $ runParserT aParser table sourceName input


-- VARIABLES

var :: IParser String
var =
  makeVar (letter <|> char '_') <?> "a name"


lowVar :: IParser String
lowVar =
  makeVar lower <?> "a lower case name"


capVar :: IParser String
capVar =
  makeVar upper <?> "an upper case name"


qualifiedVar :: IParser String
qualifiedVar =
  do  vars <- many ((++) <$> capVar <*> string ".")
      (++) (concat vars) <$> lowVar


rLabel :: IParser String
rLabel = lowVar


innerVarChar :: IParser Char
innerVarChar =
  alphaNum <|> char '_' <|> char '\'' <?> "more letters in this name"


makeVar :: IParser Char -> IParser String
makeVar firstChar =
  do  variable <- (:) <$> firstChar <*> many innerVarChar
      if variable `elem` reserveds
        then fail (Syntax.keyword variable)
        else return variable


reserved :: String -> IParser String
reserved word =
  expecting ("reserved word `" ++ word ++ "`") $
    do  string word
        notFollowedBy innerVarChar
        return word


-- INFIX OPERATORS

anyOp :: IParser String
anyOp =
  betwixt '`' '`' qualifiedVar
  <|> symOp
  <?> "an infix operator like (+)"


symOp :: IParser String
symOp =
  do  op <- many1 (satisfy Help.isSymbol)
      guard (op `notElem` [ "=", "..", "->", "--", "|", "\8594", ":" ])
      case op of
        "." -> notFollowedBy lower >> return op
        _   -> return op


-- COMMON SYMBOLS

equals :: IParser String
equals =
  string "=" <?> "="


rightArrow :: IParser String
rightArrow =
  string "->" <|> string "\8594" <?> "->"


hasType :: IParser String
hasType =
  string ":" <?> "the \"has type\" symbol ':'"


commitIf check p =
    commit <|> try p
  where
    commit =
      try (lookAhead check) >> p


-- SEPARATORS

spaceySepBy1 :: IParser b -> IParser a -> IParser [a]
spaceySepBy1 sep parser =
  do  value <- parser
      (value:) <$> spaceyPrefixBy sep parser


spaceyPrefixBy :: IParser sep -> IParser a -> IParser [a]
spaceyPrefixBy sep parser =
  many (commitIf (whitespace >> sep) (padded sep >> parser))


comma :: IParser Char
comma =
  char ',' <?> "a comma ','"


commaSep1 :: IParser a -> IParser [a]
commaSep1 =
  spaceySepBy1 comma


commaSep :: IParser a -> IParser [a]
commaSep =
  option [] . commaSep1


semiSep1 :: IParser a -> IParser [a]
semiSep1 =
  spaceySepBy1 (char ';' <?> "a semicolon ';'")


pipeSep1 :: IParser a -> IParser [a]
pipeSep1 =
  spaceySepBy1 (char '|' <?> "a vertical bar '|'")


consSep1 :: IParser a -> IParser [a]
consSep1 =
  spaceySepBy1 (string "::" <?> "a cons operator '::'")


dotSep1 :: IParser a -> IParser [a]
dotSep1 p =
  (:) <$> p <*> many (try (char '.') >> p)


spaceSep1 :: IParser a -> IParser [a]
spaceSep1 p =
  (:) <$> p <*> spacePrefix p


spacePrefix p =
  constrainedSpacePrefix p (\_ -> return ())


constrainedSpacePrefix parser constraint =
    many $ choice
      [ try (spacing >> lookAhead (oneOf "[({")) >> parser
      , try (spacing >> parser)
      ]
    where
      spacing = do
        n <- whitespace
        constraint n <?> Syntax.whitespace
        indented


-- SURROUNDED BY

followedBy a b =
  do  x <- a
      b
      return x


betwixt :: Char -> Char -> IParser a -> IParser a
betwixt a b c =
  do  char a
      out <- c
      char b <?> "a closing '" ++ [b] ++ "'"
      return out


surround :: Char -> Char -> String -> IParser a -> IParser a
surround a z name p = do
  char a
  v <- padded p
  char z <?> unwords ["a closing", name, show z]
  return v


braces :: IParser a -> IParser a
braces =
  surround '[' ']' "brace"


parens :: IParser a -> IParser a
parens =
  surround '(' ')' "paren"


brackets :: IParser a -> IParser a
brackets =
  surround '{' '}' "bracket"


-- HELPERS FOR EXPRESSIONS

getMyPosition :: IParser R.Position
getMyPosition =
  R.fromSourcePos <$> getPosition


addLocation :: IParser a -> IParser (A.Located a)
addLocation expr =
  do  (start, e, end) <- located expr
      return (A.at start end e)


located :: IParser a -> IParser (R.Position, a, R.Position)
located parser =
  do  start <- getMyPosition
      value <- parser
      end <- getMyPosition
      return (start, value, end)


accessible :: IParser Source.Expr -> IParser Source.Expr
accessible exprParser =
  do  start <- getMyPosition

      annotatedRootExpr@(A.A _ rootExpr) <- exprParser

      access <- optionMaybe (try dot <?> "a field access like .name")

      case access of
        Nothing ->
          return annotatedRootExpr

        Just _ ->
          accessible $
            do  v <- var
                end <- getMyPosition
                return . A.at start end $
                    case rootExpr of
                      E.Var (Variable.Raw name@(c:_))
                        | isUpper c ->
                            E.rawVar (name ++ '.' : v)
                      _ ->
                        E.Access annotatedRootExpr v


dot :: IParser ()
dot =
  do  char '.'
      notFollowedBy (char '.')


-- WHITESPACE

padded :: IParser a -> IParser a
padded p =
  do  whitespace
      out <- p
      whitespace
      return out


spaces :: IParser String
spaces =
  let space = string " " <|> multiComment <?> Syntax.whitespace
  in
      concat <$> many1 space


forcedWS :: IParser String
forcedWS =
  choice
    [ (++) <$> spaces <*> (concat <$> many nl_space)
    , concat <$> many1 nl_space
    ]
  where
    nl_space =
      try ((++) <$> (concat <$> many1 newline) <*> spaces)


-- Just eats whitespace until the next meaningful character.
dumbWhitespace :: IParser String
dumbWhitespace =
  concat <$> many (spaces <|> newline)


whitespace :: IParser String
whitespace =
  option "" forcedWS


freshLine :: IParser [[String]]
freshLine =
    try (many1 newline >> many space_nl) <|> try (many1 space_nl) <?> Syntax.freshLine
  where
    space_nl = try $ spaces >> many1 newline


newline :: IParser String
newline =
  simpleNewline <|> lineComment <?> Syntax.newline


simpleNewline :: IParser String
simpleNewline =
  try (string "\r\n") <|> string "\n"


lineComment :: IParser String
lineComment =
  do  try (string "--")
      comment <- anyUntil $ simpleNewline <|> (eof >> return "\n")
      return ("--" ++ comment)


docComment :: IParser String
docComment =
  do  try (string "{-|")
      contents <- closeComment
      return (init (init contents))


multiComment :: IParser String
multiComment =
  (++) <$> try (string "{-" <* notFollowedBy (string "|")) <*> closeComment


closeComment :: IParser String
closeComment =
    anyUntil $
      choice $
        [ try (string "-}") <?> "the end of a comment -}"
        , concat <$> sequence [ try (string "{-"), closeComment, closeComment ]
        ]


-- ODD COMBINATORS

failure msg = do
  inp <- getInput
  setInput ('x':inp)
  anyToken
  fail msg


until :: IParser a -> IParser b -> IParser b
until p end =
    go
  where
    go = end <|> (p >> go)


anyUntil :: IParser String -> IParser String
anyUntil end =
    go
  where
    go =
      end <|> (:) <$> anyChar <*> go


ignoreUntil :: IParser a -> IParser (Maybe a)
ignoreUntil end =
    go
  where
    ignore p =
      const () <$> p

    filler =
      choice
        [ try (ignore chr) <|> ignore str
        , ignore multiComment
        , ignore docComment
        , ignore anyChar
        ]

    go =
      choice
        [ Just <$> end
        , filler `until` choice [ const Nothing <$> eof, newline >> go ]
        ]


onFreshLines :: (a -> b -> b) -> b -> IParser a -> IParser b
onFreshLines insert init thing =
    go init
  where
    go values =
      do  optionValue <- ignoreUntil thing
          case optionValue of
            Nothing -> return values
            Just v  -> go (insert v values)


withSource :: IParser a -> IParser (String, a)
withSource p =
  do  start  <- getParserState
      result <- p
      endPos <- getPosition
      setParserState start
      raw <- anyUntilPos endPos
      return (raw, result)


anyUntilPos :: SourcePos -> IParser String
anyUntilPos pos =
  do  currentPos <- getPosition
      if currentPos == pos
        then return []
        else (:) <$> anyChar <*> anyUntilPos pos


-- BASIC LANGUAGE LITERALS

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


str :: IParser String
str =
  expecting "a string" $
  do  s <- choice [ multiStr, singleStr ]
      processAs T.stringLiteral . sandwich '\"' $ concat s
  where
    rawString quote insides =
        quote >> manyTill insides quote

    multiStr  = rawString (try (string "\"\"\"")) multilineStringChar
    singleStr = rawString (char '"') stringChar

    stringChar :: IParser String
    stringChar = choice [ newlineChar, escaped '\"', (:[]) <$> satisfy (/= '\"') ]

    multilineStringChar :: IParser String
    multilineStringChar =
        do noEnd
           choice [ newlineChar, escaped '\"', expandQuote <$> anyChar ]
        where
          noEnd = notFollowedBy (string "\"\"\"")
          expandQuote c = if c == '\"' then "\\\"" else [c]

    newlineChar :: IParser String
    newlineChar =
        choice [ char '\n' >> return "\\n"
               , char '\r' >> return "\\r" ]


sandwich :: Char -> String -> String
sandwich delim s =
  delim : s ++ [delim]


escaped :: Char -> IParser String
escaped delim =
  try $ do
    char '\\'
    c <- char '\\' <|> char delim
    return ['\\', c]


chr :: IParser Char
chr =
    betwixt '\'' '\'' character <?> "a character"
  where
    nonQuote = satisfy (/='\'')

    character =
      do  c <- choice
                [ escaped '\''
                , (:) <$> char '\\' <*> many1 nonQuote
                , (:[]) <$> nonQuote
                ]

          processAs T.charLiteral $ sandwich '\'' c


processAs :: (T.GenTokenParser String u SourceM -> IParser a) -> String -> IParser a
processAs processor s =
    calloutParser s (processor lexer)
  where
    calloutParser :: String -> IParser a -> IParser a
    calloutParser inp p =
      either (fail . show) return (iParse p inp)

    lexer :: T.GenTokenParser String u SourceM
    lexer = T.makeTokenParser elmDef

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
