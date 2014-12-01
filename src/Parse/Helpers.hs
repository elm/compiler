module Parse.Helpers where

import Prelude hiding (until)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard, join)
import Control.Monad.State (State)
import Data.Char (isUpper)
import qualified Data.Map as Map
import qualified Language.GLSL.Parser as GLP
import qualified Language.GLSL.Syntax as GLS
import Text.Parsec hiding (newline,spaces,State)
import Text.Parsec.Indent (indented, runIndent)
import qualified Text.Parsec.Token as T

import qualified AST.Annotation as Annotation
import qualified AST.Declaration as Decl
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.PrettyPrint as P
import qualified AST.Variable as Variable
import Elm.Utils ((|>))

reserveds :: [String]
reserveds =
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "as", "hiding", "open"
    , "export", "foreign"
    , "deriving", "port"
    ]


expecting = flip (<?>)


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
  makeVar (letter <|> char '_' <?> "variable")


lowVar :: IParser String
lowVar =
  makeVar (lower <?> "lower case variable")


capVar :: IParser String
capVar =
  makeVar (upper <?> "upper case variable")


qualifiedVar :: IParser String
qualifiedVar =
  do  vars <- many ((++) <$> capVar <*> string ".")
      (++) (concat vars) <$> lowVar


rLabel :: IParser String
rLabel = lowVar


innerVarChar :: IParser Char
innerVarChar =
  alphaNum <|> char '_' <|> char '\'' <?> "" 


makeVar :: IParser Char -> IParser String
makeVar p =
  do  v <- (:) <$> p <*> many innerVarChar
      if v `elem` reserveds
        then fail $ "unexpected keyword '" ++ v ++ "', variables cannot be keywords"
        else return v


reserved :: String -> IParser String
reserved word =
  (try (string word >> notFollowedBy innerVarChar) >> return word)
  <?> "reserved word '" ++ word ++ "'"


-- INFIX OPERATORS

anyOp :: IParser String
anyOp =
  betwixt '`' '`' qualifiedVar
  <|> symOp
  <?> "infix operator"


symOp :: IParser String
symOp =
  do  op <- many1 (satisfy Help.isSymbol)
      guard (op `notElem` [ "=", "..", "->", "--", "|", "\8594", ":" ])
      case op of
        "." -> notFollowedBy lower >> return op
        "\8728" -> return "."
        _   -> return op


-- COMMON SYMBOLS

equals :: IParser String
equals = string "="

arrow :: IParser String
arrow = string "->" <|> string "\8594" <?> "arrow (->)"

hasType :: IParser String
hasType = string ":" <?> "':' (a type annotation)'"


commitIf check p =
    commit <|> try p
  where
    commit =
      try (lookAhead check) >> p


-- SEPARATORS

spaceySepBy1 :: IParser b -> IParser a -> IParser [a]
spaceySepBy1 sep p =
  do  a <- p
      (a:) <$> many (commitIf (whitespace >> sep) (padded sep >> p))


commaSep1 :: IParser a -> IParser [a]
commaSep1 =
  spaceySepBy1 (char ',' <?> "comma ','")


commaSep :: IParser a -> IParser [a]
commaSep =
  option [] . commaSep1


semiSep1 :: IParser a -> IParser [a]
semiSep1 =
  spaceySepBy1 (char ';' <?> "semicolon ';'")


pipeSep1 :: IParser a -> IParser [a]
pipeSep1 =
  spaceySepBy1 (char '|' <?> "type divider '|'")


consSep1 :: IParser a -> IParser [a]
consSep1 =
  spaceySepBy1 (string "::" <?> "cons operator '::'")


dotSep1 :: IParser a -> IParser [a]
dotSep1 p =
  (:) <$> p <*> many (try (char '.') >> p)


spaceSep1 :: IParser a -> IParser [a]
spaceSep1 p =
  (:) <$> p <*> spacePrefix p


spacePrefix p =
  constrainedSpacePrefix p (\_ -> return ())


constrainedSpacePrefix p constraint =
    many $ choice
      [ try (spacing >> lookAhead (oneOf "[({")) >> p
      , try (spacing >> p)
      ]
    where
      spacing = do
        n <- whitespace
        constraint n
        indented


-- SURROUNDED BY

followedBy a b =
  do  x <- a
      b
      return x


betwixt a b c =
  do  char a
      out <- c
      char b <?> "closing '" ++ [b] ++ "'"
      return out


surround a z name p = do
  char a
  v <- padded p
  char z <?> unwords ["closing", name, show z]
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

addLocation :: (P.Pretty a) => IParser a -> IParser (Annotation.Located a)
addLocation expr =
  do  (start, e, end) <- located expr
      return (Annotation.at start end e)


located :: IParser a -> IParser (SourcePos, a, SourcePos)
located p =
  do  start <- getPosition
      e <- p
      end <- getPosition
      return (start, e, end)


accessible :: IParser Source.Expr -> IParser Source.Expr
accessible expr =
  do  start <- getPosition
      ce@(Annotation.A _ e) <- expr
      let rest f = do
            let dot = char '.' >> notFollowedBy (char '.')
            access <- optionMaybe (try dot <?> "field access (e.g. List.map)")
            case access of
              Nothing -> return ce
              Just _  -> accessible $ do
                           v <- var <?> "field access (e.g. List.map)"
                           end <- getPosition
                           return (Annotation.at start end (f v))
      case e of
        E.Var (Variable.Raw (c:cs))
            | isUpper c -> rest (\v -> E.rawVar (c:cs ++ '.':v))
            | otherwise -> rest (E.Access ce)
        _ -> rest (E.Access ce)


-- WHITESPACE

padded :: IParser a -> IParser a
padded p =
  do  whitespace
      out <- p
      whitespace
      return out


spaces :: IParser String
spaces =
  let space = string " " <|> multiComment <?> "whitespace"
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
    try (many1 newline >> many space_nl) <|> try (many1 space_nl) <?> ""
  where
    space_nl = try $ spaces >> many1 newline


newline :: IParser String
newline =
  simpleNewline <|> lineComment <?> "a newline"


simpleNewline :: IParser String
simpleNewline =
  try (string "\r\n") <|> string "\n"


lineComment :: IParser String
lineComment =
  do  try (string "--")
      comment <- anyUntil $ simpleNewline <|> (eof >> return "\n")
      return ("--" ++ comment)


multiComment :: IParser String
multiComment =
  (++) <$> try (string "{-") <*> closeComment


closeComment :: IParser String
closeComment =
    anyUntil $
      choice $
        [ try (string "-}") <?> "close comment"
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
    go
  where
    go = do currentPos <- getPosition
            case currentPos == pos of
              True -> return []
              False -> (:) <$> anyChar <*> go


-- BASIC LANGUAGE LITERALS

shader :: IParser (String, L.GLShaderTipe)
shader =
  do  try (string "[glsl|")
      rawSrc <- closeShader id
      case glSource rawSrc of
        Left err -> parserFail . show $ err
        Right tipe -> return (rawSrc, tipe)
  where
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
str = expecting "String" $ do
        s <- choice [ multiStr, singleStr ]
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
    betwixt '\'' '\'' character <?> "character"
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
        , T.reservedOpNames = [":", "->", "<-", "|"]
        , T.caseSensitive   = True
        }
