module Parse.Expression (def,term,typeAnnotation,expr) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos)

import qualified Parse.Binop as Binop
import Parse.Helpers
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import qualified AST.Annotation as Annotation
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var


--------  Basic Terms  --------

varTerm :: IParser Source.Expr'
varTerm = toVar <$> var <?> "variable"

toVar :: String -> Source.Expr'
toVar v =
    case v of
      "True"  -> E.Literal (L.Boolean True)
      "False" -> E.Literal (L.Boolean False)
      _       -> E.rawVar v

accessor :: IParser Source.Expr'
accessor = do
  (start, lbl, end) <- located (try (string "." >> rLabel))
  let loc e = Annotation.at start end e
  return (E.Lambda (P.Var "_") (loc $ E.Access (loc $ E.rawVar "_") lbl))

negative :: IParser Source.Expr'
negative = do
  (start, nTerm, end) <-
      located (try (char '-' >> notFollowedBy (char '.' <|> char '-')) >> term)
  let loc e = Annotation.at start end e
  return (E.Binop (Var.Raw "-") (loc $ E.Literal (L.IntNum 0)) nTerm)


--------  Complex Terms  --------

listTerm :: IParser Source.Expr'
listTerm = shader' <|> braces (try range <|> E.ExplicitList <$> commaSep expr)
  where
    range = do
      lo <- expr
      padded (string "..")
      E.Range lo <$> expr

    shader' = do
      pos <- getPosition
      let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
      (rawSrc, tipe) <- Help.shader
      return $ E.GLShader uid (filter (/='\r') rawSrc) tipe


parensTerm :: IParser Source.Expr
parensTerm = try (parens opFn) <|> parens (tupleFn <|> parened)
  where
    opFn = do
      (start, op, end) <- located anyOp
      let loc = Annotation.at start end
      return . loc . E.Lambda (P.Var "x") . loc . E.Lambda (P.Var "y") . loc $
             E.Binop (Var.Raw op) (loc (E.rawVar "x")) (loc (E.rawVar "y"))

    tupleFn = do
      let comma = char ',' <?> "comma ','"
      (start, commas, end) <- located (comma >> many (whitespace >> comma))
      let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
          loc = Annotation.at start end
      return $ foldr (\x e -> loc $ E.Lambda x e)
                 (loc . E.tuple $ map (loc . E.rawVar) vars) (map P.Var vars)
    
    parened = do
      (start, es, end) <- located (commaSep expr)
      return $ case es of
                 [e] -> e
                 _   -> Annotation.at start end (E.tuple es)

recordTerm :: IParser Source.Expr
recordTerm = brackets $ choice [ misc, addLocation record ]
    where
      field = do
        label <- rLabel
        patterns <- spacePrefix Pattern.term
        padded equals
        body <- expr
        return (label, makeFunction patterns body)
              
      record = E.Record <$> commaSep field

      change = do
        lbl <- rLabel
        padded (string "<-")
        (,) lbl <$> expr

      remove r = addLocation (string "-" >> whitespace >> E.Remove r <$> rLabel)

      insert r = addLocation $ do
                   string "|" >> whitespace
                   E.Insert r <$> rLabel <*> (padded equals >> expr)

      modify r =
          addLocation (string "|" >> whitespace >> E.Modify r <$> commaSep1 change)

      misc = try $ do
               record <- addLocation (E.rawVar <$> rLabel)
               opt <- padded (optionMaybe (remove record))
               case opt of
                 Just e  -> try (insert e) <|> return e
                 Nothing -> try (insert record) <|> try (modify record)
                        

term :: IParser Source.Expr
term =  addLocation (choice [ E.Literal <$> Literal.literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "basic term (4, x, 'c', etc.)"

--------  Applications  --------

appExpr :: IParser Source.Expr
appExpr = do
  t <- term
  ts <- constrainedSpacePrefix term $ \str ->
            if null str then notFollowedBy (char '-') else return ()
  return $ case ts of
             [] -> t
             _  -> List.foldl' (\f x -> Annotation.merge f x $ E.App f x) t ts

--------  Normal Expressions  --------

binaryExpr :: IParser Source.Expr
binaryExpr = Binop.binops appExpr lastExpr anyOp
  where lastExpr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
                <|> lambdaExpr

ifExpr :: IParser Source.Expr'
ifExpr = reserved "if" >> whitespace >> (normal <|> multiIf)
    where
      normal = do
        bool <- expr
        padded (reserved "then")
        thenBranch <- expr
        whitespace <?> "an 'else' branch" ; reserved "else" <?> "an 'else' branch" ; whitespace
        elseBranch <- expr
        return $ E.MultiIf
          [ (bool, thenBranch)
          , (Annotation.sameAs elseBranch (E.Literal . L.Boolean $ True), elseBranch)
          ]

      multiIf = E.MultiIf <$> spaceSep1 iff
          where iff = do string "|" ; whitespace
                         b <- expr ; padded arrow
                         (,) b <$> expr

lambdaExpr :: IParser Source.Expr
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                args <- spaceSep1 Pattern.term
                padded arrow
                body <- expr
                return (makeFunction args body)

defSet :: IParser [Source.Def]
defSet = block (do d <- def ; whitespace ; return d)

letExpr :: IParser Source.Expr'
letExpr = do
  reserved "let" ; whitespace
  defs <- defSet
  padded (reserved "in")
  E.Let defs <$> expr

caseExpr :: IParser Source.Expr'
caseExpr = do
  reserved "case"; e <- padded expr; reserved "of"; whitespace
  E.Case e <$> (with <|> without)
    where case_ = do p <- Pattern.expr
                     padded arrow
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = block (do c <- case_ ; whitespace ; return c)

expr :: IParser Source.Expr
expr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
    <|> lambdaExpr
    <|> binaryExpr 
    <?> "an expression"

defStart :: IParser [P.RawPattern]
defStart =
    choice [ do p1 <- try Pattern.term
                infics p1 <|> func p1
           , func =<< (P.Var <$> parens symOp)
           , (:[]) <$> Pattern.expr
           ] <?> "the definition of a variable (x = ...)"
    where
      func pattern =
          case pattern of
            P.Var _ -> (pattern:) <$> spacePrefix Pattern.term
            _ -> do try (lookAhead (whitespace >> string "="))
                    return [pattern]

      infics p1 = do
        o:p <- try (whitespace >> anyOp)
        p2  <- (whitespace >> Pattern.term)
        return $ if o == '`' then [ P.Var $ takeWhile (/='`') p, p1, p2 ]
                             else [ P.Var (o:p), p1, p2 ]

makeFunction :: [P.RawPattern] -> Source.Expr -> Source.Expr
makeFunction args body@(Annotation.A ann _) =
    foldr (\arg body' -> Annotation.A ann $ E.Lambda arg body') body args

definition :: IParser Source.Def
definition = withPos $ do
  (name:args) <- defStart
  padded equals
  body <- expr
  return . Source.Definition name $ makeFunction args body

typeAnnotation :: IParser Source.Def
typeAnnotation = Source.TypeAnnotation <$> try start <*> Type.expr
  where
    start = do
      v <- lowVar <|> parens symOp
      padded hasType
      return v

def :: IParser Source.Def
def = typeAnnotation <|> definition
