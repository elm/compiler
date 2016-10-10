module Parse.Expression (term, expr, def) where

import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)
import qualified Text.Parsec.Indent as Indent

import qualified Parse.Binop as Binop
import Parse.Helpers
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A



--------  Basic Terms  --------


varTerm :: IParser Source.Expr'
varTerm =
  toVar <$> var


toVar :: String -> Source.Expr'
toVar v =
  case v of
    "True" ->
        E.Literal (L.Boolean True)

    "False" ->
        E.Literal (L.Boolean False)

    _ ->
        E.rawVar v


accessor :: IParser Source.Expr'
accessor =
  do  (start, lbl, end) <- located (try (string "." >> rLabel))

      let ann value =
            A.at start end value

      return $
        E.Lambda
            (ann (P.Var "_"))
            (ann (E.Access (ann (E.rawVar "_")) lbl))


negative :: IParser Source.Expr'
negative =
  do  (start, nTerm, end) <-
          located $ try $
            do  char '-'
                notFollowedBy (char '.' <|> char '-')
                term

      let ann e =
            A.at start end e

      return $
        E.Binop
          (Var.Raw "-")
          (ann (E.Literal (L.IntNum 0)))
          nTerm



--------  Complex Terms  --------


listTerm :: IParser Source.Expr'
listTerm =
  choice
    [ do  pos <- getPosition
          let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
          (rawSrc, tipe) <- Help.shader
          return $ E.GLShader uid (filter (/='\r') rawSrc) tipe
    , braces (E.ExplicitList <$> commaSep expr)
    ]


parensTerm :: IParser Source.Expr
parensTerm =
  let
    mkFunc args realBody start end =
      foldr
        (\arg body -> A.at start end (E.Lambda (A.at start end (P.Var arg)) body))
        (A.at start end (realBody (A.at start end . E.rawVar)))
        args

    mkBinop op =
      mkFunc ["x","y"] (\var -> E.Binop (Var.Raw op) (var "x") (var "y"))

    tupleFn =
      do  commas <- many1 comma
          let args = map (('v':) . show) [ 0 .. length commas ]
          return $ mkFunc args (\var -> E.tuple (map var args))

    parenedExpr =
      do  expressions <- padded (commaSep expr)
          return $ \start end ->
              case expressions of
                [expression] ->
                    expression
                _ ->
                    A.at start end (E.tuple expressions)
  in
    do  (start, mkExpr, end) <-
          located $ choice $
            [ mkBinop <$> try (parens Binop.infixOp)
            , parens (tupleFn <|> parenedExpr)
            ]
        return (mkExpr start end)


recordTerm :: IParser Source.Expr
recordTerm =
  addLocation $ brackets $ choice $
    [ do  starter <- try (addLocation rLabel)
          whitespace
          choice
            [ update starter
            , literal starter
            ]
    , return (E.Record [])
    ]
  where
    update (A.A ann starter) =
      do  try (string "|")
          whitespace
          fields <- commaSep1 field
          return (E.Update (A.A ann (E.rawVar starter)) fields)

    literal (A.A _ starter) =
      do  try equals
          whitespace
          value <- expr
          whitespace
          choice
            [ do  try comma
                  whitespace
                  fields <- commaSep field
                  return (E.Record ((starter, value) : fields))
            , return (E.Record [(starter, value)])
            ]

    field =
      do  key <- rLabel
          padded equals
          value <- expr
          return (key, value)


term :: IParser Source.Expr
term =
  addLocation (choice [ E.Literal <$> Literal.literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "an expression"



--------  Applications  --------


appExpr :: IParser Source.Expr
appExpr =
  expecting "an expression" $
  do  t <- term
      ts <- constrainedSpacePrefix term $ \str ->
                if null str then notFollowedBy (char '-') else return ()
      return $
          case ts of
            [] -> t
            _  -> List.foldl' (\f x -> A.merge f x $ E.App f x) t ts



--------  Normal Expressions  --------


expr :: IParser Source.Expr
expr =
  addLocation (choice [ letExpr, caseExpr, ifExpr ])
    <|> lambdaExpr
    <|> binaryExpr
    <?> "an expression"


binaryExpr :: IParser Source.Expr
binaryExpr =
    Binop.binops appExpr lastExpr
  where
    lastExpr =
        addLocation (choice [ letExpr, caseExpr, ifExpr ])
        <|> lambdaExpr
        <?> "an expression"


ifExpr :: IParser Source.Expr'
ifExpr =
  ifHelp []


ifHelp :: [(Source.Expr, Source.Expr)] -> IParser Source.Expr'
ifHelp branches =
  do  try (reserved "if")
      whitespace
      condition <- expr
      padded (reserved "then")
      thenBranch <- expr
      whitespace <?> "an 'else' branch"
      reserved "else" <?> "an 'else' branch"
      whitespace
      let newBranches = (condition, thenBranch) : branches
      choice
        [ ifHelp newBranches
        , E.If (reverse newBranches) <$> expr
        ]


lambdaExpr :: IParser Source.Expr
lambdaExpr =
  do  start <- getMyPosition
      char '\\' <|> char '\x03BB' <?> "an anonymous function"
      whitespace
      args <- spaceSep1 Pattern.term
      padded rightArrow
      body <- expr
      end <- getMyPosition
      return (foldr (\a b -> A.at start end $ E.Lambda a b) body args)


caseExpr :: IParser Source.Expr'
caseExpr =
  do  try (reserved "case")
      exp <- padded expr
      reserved "of"
      whitespace
      Indent.withPos $
        do  firstBranch <- branch
            branches <-
                many $ do
                  try (whitespace >> Indent.checkIndent)
                  branch
            return $ E.Case exp (firstBranch : branches)
  where
    branch =
      do  p <- Pattern.expr
          padded rightArrow
          (,) p <$> expr



-- LET


letExpr :: IParser Source.Expr'
letExpr =
  do  try (reserved "let")
      whitespace
      defs <- Indent.block (def <* whitespace)
      whitespace
      reserved "in"
      whitespace
      E.Let defs <$> expr


def :: IParser Source.Def
def =
  addLocation $ Indent.withPos $
    do  start <- defStart
        whitespace
        case start of
          A.A _ (P.Var name) ->
            choice
              [ do  hasType
                    whitespace
                    tipe <- Type.expr
                    return $ Source.Annotation name tipe
              , defEnd start []
              ]

          _ ->
            defEnd start []


defStart :: IParser P.Raw
defStart =
  choice
    [ addLocation (try (P.Var <$> parens Binop.infixOp))
    , Pattern.term
    ]


defEnd :: P.Raw -> [P.Raw] -> IParser Source.Def'
defEnd start revArgs =
  choice
    [ do  arg <- Pattern.term
          whitespace
          defEnd start (arg : revArgs)
    , do  equals
          whitespace
          body <- expr
          return $ Source.Definition start (makeFunction revArgs body)
    ]


makeFunction :: [P.Raw] -> Source.Expr -> Source.Expr
makeFunction revArgs body@(A.A ann _) =
  List.foldl' (\expr arg -> A.A ann $ E.Lambda arg expr) body revArgs
