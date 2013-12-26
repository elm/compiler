module Parse.Expression (def,term,typeAnnotation) where

import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Helpers
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type
import Parse.Binop
import Parse.Literal

import SourceSyntax.Location as Location
import SourceSyntax.Pattern hiding (tuple,list)
import qualified SourceSyntax.Literal as Literal
import SourceSyntax.Expression
import SourceSyntax.Declaration (Declaration(Definition))


--------  Basic Terms  --------

varTerm :: IParser (Expr t v)
varTerm = toVar <$> var <?> "variable"

toVar :: String -> Expr t v
toVar v = case v of "True"  -> Literal (Literal.Boolean True)
                    "False" -> Literal (Literal.Boolean False)
                    _       -> Var v

accessor :: IParser (Expr t v)
accessor = do
  (start, lbl, end) <- located (try (string "." >> rLabel))
  let loc e = Location.at start end e
  return (Lambda (PVar "_") (loc $ Access (loc $ Var "_") lbl))

negative :: IParser (Expr t v)
negative = do
  (start, nTerm, end) <-
      located (try (char '-' >> notFollowedBy (char '.' <|> char '-')) >> term)
  let loc e = Location.at start end e
  return (Binop "-" (loc $ Literal (Literal.IntNum 0)) nTerm)


--------  Complex Terms  --------

listTerm :: IParser (Expr t v)
listTerm = markdown' <|> braces (try range <|> ExplicitList <$> commaSep expr)
  where
    range = do
      lo <- expr
      padded (string "..")
      Range lo <$> expr

    markdown' = do
      pos <- getPosition
      let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
      (rawText, exprs) <- markdown (interpolation uid)
      return (Markdown uid (filter (/='\r') rawText) exprs)

    span uid index =
        "<span id=\"md-" ++ uid ++ "-" ++ show index ++ "\">{{ markdown interpolation is in the pipeline, but still needs more testing }}</span>"

    interpolation uid md exprs = do
      try (string "{{")
      e <- padded expr
      string "}}"
      return (md ++ span uid (length exprs), exprs ++ [e])

parensTerm :: IParser (LExpr t v)
parensTerm = try (parens opFn) <|> parens (tupleFn <|> parened)
  where
    opFn = do
      (start, op, end) <- located anyOp
      let loc = Location.at start end
      return . loc . Lambda (PVar "x") . loc . Lambda (PVar "y") . loc $
             Binop op (loc $ Var "x") (loc $ Var "y")

    tupleFn = do
      let comma = char ',' <?> "comma ','"
      (start, commas, end) <- located (comma >> many (whitespace >> comma))
      let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
          loc = Location.at start end
      return $ foldr (\x e -> loc $ Lambda x e)
                 (loc . tuple $ map (loc . Var) vars) (map PVar vars)
    
    parened = do
      (start, es, end) <- located (commaSep expr)
      return $ case es of
                 [e] -> e
                 _   -> Location.at start end (tuple es)

recordTerm :: IParser (LExpr t v)
recordTerm = brackets $ choice [ misc, addLocation record ]
    where field = do
              label <- rLabel
              patterns <- spacePrefix Pattern.term
              padded equals
              body <- expr
              return (label, makeFunction patterns body)
              
          record = Record <$> commaSep field

          change = do
              lbl <- rLabel
              padded (string "<-")
              (,) lbl <$> expr

          remove r = addLocation (string "-" >> whitespace >> Remove r <$> rLabel)

          insert r = addLocation $ do
                       string "|" >> whitespace
                       Insert r <$> rLabel <*> (padded equals >> expr)

          modify r = addLocation
                     (string "|" >> whitespace >> Modify r <$> commaSep1 change)

          misc = try $ do
            record <- addLocation (Var <$> rLabel)
            opt <- padded (optionMaybe (remove record))
            case opt of
              Just e  -> try (insert e) <|> return e
              Nothing -> try (insert record) <|> try (modify record)
                        

term :: IParser (LExpr t v)
term =  addLocation (choice [ Literal <$> literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr :: IParser (LExpr t v)
appExpr = do
  t <- term
  ts <- constrainedSpacePrefix term $ \str ->
            if null str then notFollowedBy (char '-') else return ()
  return $ case ts of
             [] -> t
             _  -> foldl' (\f x -> Location.merge f x $ App f x) t ts

--------  Normal Expressions  --------

binaryExpr :: IParser (LExpr t v)
binaryExpr = binops appExpr lastExpr anyOp
  where lastExpr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
                <|> lambdaExpr

ifExpr :: IParser (Expr t v)
ifExpr = reserved "if" >> whitespace >> (normal <|> multiIf)
    where
      normal = do
        bool <- expr
        padded (reserved "then")
        thenBranch <- expr
        whitespace <?> "an 'else' branch" ; reserved "else" <?> "an 'else' branch" ; whitespace
        elseBranch <- expr
        return $ MultiIf [(bool, thenBranch),
                          (Location.sameAs elseBranch (Literal . Literal.Boolean $ True), elseBranch)]
      multiIf = MultiIf <$> spaceSep1 iff
          where iff = do string "|" ; whitespace
                         b <- expr ; padded arrow
                         (,) b <$> expr

lambdaExpr :: IParser (LExpr t v)
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                args <- spaceSep1 Pattern.term
                padded arrow
                body <- expr
                return (makeFunction args body)

defSet :: IParser [Def t v]
defSet = block (do d <- def ; whitespace ; return d)

letExpr :: IParser (Expr t v)
letExpr = do
  reserved "let" ; whitespace
  defs <- defSet
  padded (reserved "in")
  Let defs <$> expr

caseExpr :: IParser (Expr t v)
caseExpr = do
  reserved "case"; e <- padded expr; reserved "of"; whitespace
  Case e <$> (with <|> without)
    where case_ = do p <- Pattern.expr
                     padded arrow
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = block (do c <- case_ ; whitespace ; return c)

expr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
    <|> lambdaExpr
    <|> binaryExpr 
    <?> "an expression"

defStart :: IParser [Pattern]
defStart =
    choice [ do p1 <- try Pattern.term
                infics p1 <|> func p1
           , func =<< (PVar <$> parens symOp)
           , (:[]) <$> Pattern.expr
           ] <?> "the definition of a variable (x = ...)"
    where
      func pattern =
          case pattern of
            PVar v -> (pattern:) <$> spacePrefix Pattern.term
            _ -> do try (lookAhead (whitespace >> string "="))
                    return [pattern]

      infics p1 = do
        o:p <- try (whitespace >> anyOp)
        p2  <- (whitespace >> Pattern.term)
        return $ if o == '`' then [ PVar $ takeWhile (/='`') p, p1, p2 ]
                             else [ PVar (o:p), p1, p2 ]

makeFunction :: [Pattern] -> LExpr t v -> LExpr t v
makeFunction args body@(L s _) =
    foldr (\arg body' -> L s $ Lambda arg body') body args

definition :: IParser (Def t v)
definition = withPos $ do
  (name:args) <- defStart
  padded equals
  body <- expr
  return . Def name $ makeFunction args body

typeAnnotation :: IParser (Def t v)
typeAnnotation = TypeAnnotation <$> try start <*> Type.expr
  where
    start = do
      v <- lowVar <|> parens symOp
      padded hasType
      return v

def :: IParser (Def t v)
def = typeAnnotation <|> definition
