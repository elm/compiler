{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Helpers
  ( module Parse.Primitives
  , var, lowVar, capVar, rLabel, reserved, reserveds
  , equals, rightArrow, hasType
  , commitIf, followedBy, anyUntil
  , comma, commaSep1, commaSep
  , pipeSep1, consSep1, dotSep1, spaceSep1, spacePrefix, constrainedSpacePrefix
  , braces, parens, brackets
  , accessible
  , getMyPosition, addLocation, located
  , whitespace, freshLine, simpleNewline, newline, spaces
  , forcedWS, padded, dumbWhitespace, docComment
  )
  where

import Data.Char (isUpper)
import Text.Parsec hiding (newline, space, spaces)

import qualified AST.Expression.Source as Src
import qualified AST.Variable as Variable
import Parse.Primitives
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Region as R



-- VARIABLES


var :: IParser String
var =
  makeVar letter <?> "a name"


lowVar :: IParser String
lowVar =
  makeVar lower <?> "a lower case name"


capVar :: IParser String
capVar =
  makeVar upper <?> "an upper case name"


rLabel :: IParser String
rLabel =
  lowVar


innerVarChar :: IParser Char
innerVarChar =
  alphaNum <|> char '_' <?> "more letters in this name"


makeVar :: IParser Char -> IParser String
makeVar firstChar =
  do  variable <- (:) <$> firstChar <*> many innerVarChar
      choice
        [ do  lookAhead (char '\'')
              failure (Syntax.prime variable)
        , if variable `elem` reserveds then
            failure (Syntax.keyword variable)
          else
            return variable
        ]


reserved :: String -> IParser String
reserved word =
  expecting ("reserved word `" ++ word ++ "`") $
    do  string word
        choice
          [ do  lookAhead (char '\'')
                failure (Syntax.prime word)
          , do  notFollowedBy innerVarChar
                return word
          ]


reserveds :: [String]
reserveds =
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "exposing"
    , "as"
    , "port"
    ]



-- COMMON SYMBOLS


equals :: IParser String
equals =
  string "=" <?> "an equals sign '='"


rightArrow :: IParser String
rightArrow =
  string "->" <?> "an arrow '->'"


hasType :: IParser String
hasType =
  string ":" <?> "the \"has type\" symbol ':'"



-- WEIRD COMBINATORS


commitIf :: IParser a -> IParser b -> IParser b
commitIf check p =
    commit <|> try p
  where
    commit =
      try (lookAhead check) >> p


anyUntil :: IParser () -> IParser ()
anyUntil end =
  let
    go =
      end <|> (anyChar >> go)
  in
    go


followedBy :: IParser a -> IParser b -> IParser a
followedBy a b =
  do  x <- a
      b
      return x



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


spacePrefix :: IParser a -> IParser [a]
spacePrefix p =
  constrainedSpacePrefix p (\_ -> return ())


constrainedSpacePrefix :: IParser a -> (Bool -> IParser ()) -> IParser [a]
constrainedSpacePrefix parser constraint =
    many $ choice
      [ try (spacing >> lookAhead (oneOf "[({\"'")) >> parser
      , try (spacing >> parser)
      ]
    where
      spacing = do
        bool <- whitespace
        constraint bool
        indented



-- SURROUNDED BY


surround :: Char -> Char -> String -> IParser a -> IParser a
surround start end name parser =
  do  char start
      whitespace
      value <- parser
      whitespace
      char end <?> unwords ["a closing", name, show end]
      return value


braces :: IParser a -> IParser a
braces parser =
  surround '[' ']' "brace" parser


parens :: IParser a -> IParser a
parens parser =
  surround '(' ')' "paren" parser


brackets :: IParser a -> IParser a
brackets parser =
  surround '{' '}' "bracket" parser



-- HELPERS FOR EXPRESSIONS


getMyPosition :: IParser R.Position
getMyPosition =
  R.fromSourcePos <$> getPosition


addLocation :: IParser a -> IParser (A.Located a)
addLocation parser =
  do  start <- getMyPosition
      value <- parser
      end <- getMyPosition
      return (A.at start end value)


located :: IParser a -> IParser (R.Position, a, R.Position)
located parser =
  do  start <- getMyPosition
      value <- parser
      end <- getMyPosition
      return (start, value, end)


accessible :: IParser Src.RawExpr -> IParser Src.RawExpr
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
                      Src.Var (Variable.Raw name@(c:_))
                        | isUpper c ->
                            Src.var (name ++ '.' : v)
                      _ ->
                        Src.Access annotatedRootExpr v


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


plus :: IParser a -> IParser ()
plus parser =
  parser >> star parser


star :: IParser a -> IParser ()
star parser =
  choice
    [ parser >> star parser
    , return ()
    ]


spaces :: IParser ()
spaces =
  plus $ expecting Syntax.whitespace $
    choice
      [ string " " >> return ()
      , multiComment
      ]


forcedWS :: IParser ()
forcedWS =
  do  failIfTabFound
      plus (spaces <|> newline)
      failIfTabFound
      column <- sourceColumn <$> getPosition
      if column == 1
        then fail badWhiteSpaceMessage
        else return ()


badWhiteSpaceMessage :: String
badWhiteSpaceMessage =
  "I need whitespace, but got stuck on what looks like a new declaration.\n"
  ++ "\n"
  ++ "You are either missing some stuff in the declaration above or just need to add\n"
  ++ "some spaces here:"


failIfTabFound :: IParser ()
failIfTabFound =
  do  foundTab <- option False (lookAhead (char '\t') >> return True) <?> Syntax.tab
      if foundTab
        then fail Syntax.tab
        else return ()


-- Just eats whitespace until the next meaningful character.
dumbWhitespace :: IParser ()
dumbWhitespace =
  star (spaces <|> newline)


whitespace :: IParser Bool
whitespace =
  choice
    [ forcedWS >> return True
    , return False
    ]


freshLine :: IParser ()
freshLine =
  expecting Syntax.freshLine $ try $
    do  plus (spaces <|> newline)
        column <- sourceColumn <$> getPosition
        if column == 1
          then return ()
          else fail badFreshLineMessage


badFreshLineMessage :: String
badFreshLineMessage =
  "I need a fresh line to start a new declaration. This means a new line that\n"
  ++ "starts with stuff, not with spaces or comments."


newline :: IParser ()
newline =
  simpleNewline <|> lineComment <?> Syntax.newline


simpleNewline :: IParser ()
simpleNewline =
  do  try (string "\r\n") <|> string "\n"
      return ()


lineComment :: IParser ()
lineComment =
  do  try (string "--")
      anyUntil (simpleNewline <|> eof)


multiComment :: IParser ()
multiComment =
  do  try (string "{-" >> notFollowedBy (string "|"))
      closeComment


closeComment :: IParser ()
closeComment =
  choice
    [ expecting "the end of a comment -}" $
      do  try (string "-}")
          return ()
    , do  try (string "{-")
          closeComment
          closeComment
    , do  anyChar
          closeComment
    ]


docComment :: IParser String
docComment =
  do  try (string "{-|")
      builder <- closeDocComment id
      return (builder "")


closeDocComment :: (String -> String) -> IParser (String -> String)
closeDocComment builder =
  choice
    [ expecting "the end of a comment -}" $
      do  try (string "-}")
          return builder
    , do  try (string "{-")
          addComment <- closeDocComment (\end -> "{-" ++ end)
          closeDocComment (\end -> builder (addComment ("-}" ++ end)))
    , do  c <- anyChar
          closeDocComment (\end -> builder (c:end))
    ]