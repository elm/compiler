module Parse.Expression (def,term) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent
import qualified Text.Pandoc as Pan

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

import Unique


--------  Basic Terms  --------

varTerm :: IParser (Expr t v)
varTerm = toVar <$> var <?> "variable"

toVar :: String -> Expr t v
toVar v = case v of "True"  -> Literal (Literal.Boolean True)
                    "False" -> Literal (Literal.Boolean False)
                    _       -> Var v

accessor :: IParser (Expr t v)
accessor = do
  start <- getPosition
  lbl <- try (string "." >> rLabel)
  end <- getPosition
  let loc e = Location.at start end e
  return (Lambda (PVar "_") (loc $ Access (loc $ Var "_") lbl))

negative :: IParser (Expr t v)
negative = do
  start <- getPosition
  nTerm <- try (char '-' >> notFollowedBy (char '.' <|> char '-')) >> term
  end <- getPosition
  let loc e = Location.at start end e
  return (Binop "-" (loc $ Literal (Literal.IntNum 0)) nTerm)


--------  Complex Terms  --------

listTerm :: IParser (Expr t v)
listTerm =
      (do { try $ string "[markdown|"
          ; md <- filter (/='\r') <$> manyTill anyChar (try $ string "|]")
          ; return . Markdown $ Pan.readMarkdown Pan.def md })
  <|> (braces $ choice
       [ try $ do { lo <- expr; whitespace; string ".." ; whitespace
                  ; Range lo <$> expr }
       , ExplicitList <$> commaSep expr
       ])

parensTerm :: IParser (LExpr t v)
parensTerm = try (parens opFn) <|> parens (tupleFn <|> parened)
  where
    opFn = do
      start <- getPosition
      op <- anyOp
      end <- getPosition
      let loc = Location.at start end
      return . loc . Lambda (PVar "x") . loc . Lambda (PVar "y") . loc $
             Binop op (loc $ Var "x") (loc $ Var "y")

    tupleFn = do
      start <- getPosition
      let comma = char ',' <?> "comma ','"
      commas <- comma >> many (whitespace >> comma)
      end <- getPosition
      let vars = map (('v':) . show) [ 0 .. length commas + 1 ]
          loc = Location.at start end
      return $ foldr (\x e -> loc $ Lambda x e)
                 (loc . tuple $ map (loc . Var) vars) (map PVar vars)
    
    parened = do
      start <- getPosition
      es <- commaSep expr
      end <- getPosition
      return $ case es of [e] -> e
                          _   -> Location.at start end (tuple es)

recordTerm :: IParser (LExpr t v)
recordTerm = brackets $ choice [ misc, addLocation record ]
    where field = do
              label <- rLabel
              patterns <- spacePrefix Pattern.term
              whitespace >> string "=" >> whitespace
              body <- expr
              return (label, makeFunction patterns body)
              
          record = Record <$> commaSep field

          change = do
              lbl <- rLabel
              whitespace >> string "<-" >> whitespace
              (,) lbl <$> expr

          remove r = addLocation (string "-" >> whitespace >> Remove r <$> rLabel)

          insert r = addLocation $ do
                       string "|" >> whitespace
                       Insert r <$> rLabel <*>
                           (whitespace >> string "=" >> whitespace >> expr)

          modify r = addLocation
                     (string "|" >> whitespace >> Modify r <$> commaSep1 change)

          misc = try $ do
            record <- addLocation (Var <$> rLabel)
            whitespace
            opt <- optionMaybe (remove record)
            whitespace
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
binaryExpr = binops [] appExpr lastExpr anyOp
  where lastExpr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
                <|> lambdaExpr

ifExpr :: IParser (Expr t v)
ifExpr = reserved "if" >> whitespace >> (normal <|> multiIf)
    where
      normal = do
        bool <- expr
        whitespace ; reserved "then" ; whitespace
        thenBranch <- expr
        whitespace <?> "an 'else' branch" ; reserved "else" <?> "an 'else' branch" ; whitespace
        elseBranch <- expr
        return $ MultiIf [(bool, thenBranch),
                          (Location.sameAs elseBranch (Var "otherwise"), elseBranch)]
      multiIf = MultiIf <$> spaceSep1 iff
          where iff = do string "|" ; whitespace
                         b <- expr ; whitespace ; string "->" ; whitespace
                         (,) b <$> expr

lambdaExpr :: IParser (LExpr t v)
lambdaExpr = do char '\\' <|> char '\x03BB' <?> "anonymous function"
                whitespace
                args <- spaceSep1 Pattern.term
                whitespace ; arrow ; whitespace
                body <- expr
                return (makeFunction args body)

defSet :: IParser [Def t v]
defSet = block (do d <- def ; whitespace ; return d)

letExpr :: IParser (Expr t v)
letExpr = do
  reserved "let" ; whitespace
  defs <- defSet
  whitespace ; reserved "in" ; whitespace
  Let defs <$> expr

caseExpr :: IParser (Expr t v)
caseExpr = do
  reserved "case"; whitespace; e <- expr; whitespace; reserved "of"; whitespace
  Case e <$> (with <|> without)
    where case_ = do p <- Pattern.expr; whitespace; arrow; whitespace
                     (,) p <$> expr
          with    = brackets (semiSep1 (case_ <?> "cases { x -> ... }"))
          without = block (do c <- case_ ; whitespace ; return c)

expr = addLocation (choice [ ifExpr, letExpr, caseExpr ])
    <|> lambdaExpr
    <|> binaryExpr 
    <?> "an expression"

funcDef =
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

assignExpr :: IParser (Def t v)
assignExpr = withPos $ do
  (name:args) <- funcDef
  whitespace >> string "=" >> whitespace
  body <- expr
  return . Def name $ makeFunction args body

typeAnnotation :: IParser (Def t v)
typeAnnotation = TypeAnnotation <$> try start <*> Type.expr
    where
      start = do v <- lowVar <|> parens symOp
                 whitespace ; hasType ; whitespace
                 return v

def :: IParser (Def t v)
def = typeAnnotation <|> assignExpr
