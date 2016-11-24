module Parse.Expression (term, expr, def) where

import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)

import qualified Parse.Binop as Binop
import Parse.Helpers
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import qualified AST.Expression.Source as Src
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified Reporting.Annotation as A



--------  Basic Terms  --------


varTerm :: IParser Src.RawExpr'
varTerm =
  toVar <$> Help.var


toVar :: String -> Src.RawExpr'
toVar v =
  case v of
    "True" ->
        Src.Literal (L.Boolean True)

    "False" ->
        Src.Literal (L.Boolean False)

    _ ->
        Src.var v


accessor :: IParser Src.RawExpr'
accessor =
  do  start <- getMyPosition
      label <- try (char '.' >> rLabel)
      end <- getMyPosition

      let ann value =
            A.at start end value

      return $
        Src.Lambda
            (ann (P.Var "_"))
            (ann (Src.Access (ann (Src.var "_")) label))


negative :: IParser Src.RawExpr'
negative =
  try $
    do  (A.A region _) <- addLocation (char '-')
        notFollowedBy (char '.' <|> char '-')
        negativeTerm <- term

        let zero = A.A region (Src.Literal (L.IntNum 0))
        let minus = A.A region "-"

        return $ Src.Binop [(zero, minus)] negativeTerm




--------  Complex Terms  --------


listTerm :: IParser Src.RawExpr'
listTerm =
  choice
    [ do  pos <- getPosition
          let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
          (rawSrc, tipe) <- Literal.shader
          return $ Src.GLShader uid (filter (/='\r') rawSrc) tipe
    , braces (Src.List <$> commaSep expr)
    ]


parensTerm :: IParser Src.RawExpr
parensTerm =
  let
    mkFunc args realBody start end =
      foldr
        (\arg body -> A.at start end (Src.Lambda (A.at start end (P.Var arg)) body))
        (A.at start end (realBody (A.at start end . Src.var)))
        args

    mkBinop op =
      mkFunc ["x","y"] $ \var -> Src.Binop [(var "x", op)] (var "y")

    tupleFn =
      do  commas <- many1 comma
          let args = map (('v':) . show) [ 0 .. length commas ]
          return $ mkFunc args (\var -> Src.tuple (map var args))

    parenedExpr =
      do  expressions <- padded (commaSep expr)
          return $ \start end ->
              case expressions of
                [expression] ->
                    expression
                _ ->
                    A.at start end (Src.tuple expressions)
  in
    do  char '('
        start <- getMyPosition
        mkExpr <-
          choice
            [ mkBinop <$> try (addLocation Binop.infixOp)
            , tupleFn
            , parenedExpr
            ]
        end <- getMyPosition
        char ')'
        return (mkExpr start end)


recordTerm :: IParser Src.RawExpr
recordTerm =
  addLocation $ brackets $ choice $
    [ do  starter <- try (addLocation rLabel)
          whitespace
          choice
            [ update starter
            , literal starter
            ]
    , return (Src.Record [])
    ]
  where
    update (A.A ann starter) =
      do  try (string "|")
          whitespace
          fields <- commaSep1 field
          return (Src.Update (A.A ann (Src.var starter)) fields)

    literal (A.A _ starter) =
      do  try equals
          whitespace
          value <- expr
          whitespace
          choice
            [ do  try comma
                  whitespace
                  fields <- commaSep field
                  return (Src.Record ((starter, value) : fields))
            , return (Src.Record [(starter, value)])
            ]

    field =
      do  key <- rLabel
          padded equals
          value <- expr
          return (key, value)


term :: IParser Src.RawExpr
term =
  choice
    [ addLocation $ choice $
        [ Src.Literal <$> Literal.literal
        , listTerm
        , accessor
        , negative
        ]
    , accessible $ choice $
        [ addLocation varTerm
        , parensTerm
        , recordTerm
        ]
    ]
    <?> "an expression"



--------  Applications  --------


appExpr :: IParser Src.RawExpr
appExpr =
  expecting "an expression" $
  do  t <- term
      ts <- constrainedSpacePrefix term $ \hasSpace ->
                if hasSpace then return () else notFollowedBy (char '-')
      return $
          case ts of
            [] -> t
            _  -> List.foldl' (\f x -> A.merge f x $ Src.App f x) t ts



--------  Normal Expressions  --------


expr :: IParser Src.RawExpr
expr =
  addLocation (choice [ letExpr, caseExpr, ifExpr ])
    <|> lambdaExpr
    <|> binaryExpr
    <?> "an expression"


binaryExpr :: IParser Src.RawExpr
binaryExpr =
    Binop.binops appExpr lastExpr
  where
    lastExpr =
        addLocation (choice [ letExpr, caseExpr, ifExpr ])
        <|> lambdaExpr
        <?> "an expression"


ifExpr :: IParser Src.RawExpr'
ifExpr =
  ifHelp []


ifHelp :: [(Src.RawExpr, Src.RawExpr)] -> IParser Src.RawExpr'
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
        , Src.If (reverse newBranches) <$> expr
        ]


lambdaExpr :: IParser Src.RawExpr
lambdaExpr =
  do  start <- getMyPosition
      char '\\' <|> char '\x03BB' <?> "an anonymous function"
      whitespace
      args <- spaceSep1 Pattern.term
      padded rightArrow
      body <- expr
      end <- getMyPosition
      return (foldr (\a b -> A.at start end $ Src.Lambda a b) body args)


caseExpr :: IParser Src.RawExpr'
caseExpr =
  do  try (reserved "case")
      exp <- padded expr
      reserved "of"
      whitespace
      Help.withPos $
        do  firstBranch <- branch
            branches <-
                many $ do
                  try (whitespace >> Help.checkIndent)
                  branch
            return $ Src.Case exp (firstBranch : branches)
  where
    branch =
      do  p <- Pattern.expr
          padded rightArrow
          (,) p <$> expr



-- LET


letExpr :: IParser Src.RawExpr'
letExpr =
  do  try (reserved "let")
      whitespace
      defs <- Help.block (def <* whitespace)
      whitespace
      reserved "in"
      whitespace
      Src.Let defs <$> expr


def :: IParser Src.RawDef
def =
  addLocation $ Help.withPos $
    do  start <- defStart
        whitespace
        case start of
          A.A _ (P.Var name) ->
            choice
              [ do  hasType
                    whitespace
                    tipe <- Type.expr
                    return $ Src.Annotation name tipe
              , defEnd start []
              ]

          _ ->
            defEnd start []


defStart :: IParser P.Raw
defStart =
  choice
    [ addLocation $ try $
        do  char '('
            op <- Binop.infixOp
            char ')'
            return (P.Var op)
    , Pattern.term
    ]


defEnd :: P.Raw -> [P.Raw] -> IParser Src.RawDef'
defEnd start revArgs =
  choice
    [ do  arg <- Pattern.term
          whitespace
          defEnd start (arg : revArgs)
    , do  equals
          whitespace
          body <- expr
          return $ Src.Definition start (makeFunction revArgs body)
    ]


makeFunction :: [P.Raw] -> Src.RawExpr -> Src.RawExpr
makeFunction revArgs body@(A.A ann _) =
  List.foldl' (\expr arg -> A.A ann $ Src.Lambda arg expr) body revArgs
