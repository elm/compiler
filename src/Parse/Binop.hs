module Parse.Binop (binops, infixOp) where

import Text.Parsec
  ( (<|>), choice, lookAhead, lower, many1
  , notFollowedBy, satisfy, string, try
  )

import qualified AST.Expression.Source as Src
import qualified AST.Helpers as Help
import Parse.Helpers (IParser, addLocation, commitIf, expecting, failure, whitespace)
import qualified Reporting.Annotation as A



-- BINARY EXPRESSIONS


binops :: IParser Src.RawExpr -> IParser Src.RawExpr -> IParser Src.RawExpr
binops parseTerm parseLastTerm =
  addLocation $
    do  expr <- parseTerm
        (ops, last) <- binopsHelp parseTerm parseLastTerm expr
        case ops of
          [] ->
            return (A.drop last)

          _ ->
            return (Src.Binop ops last)


binopsHelp
  :: IParser Src.RawExpr
  -> IParser Src.RawExpr
  -> Src.RawExpr
  -> IParser ([(Src.RawExpr, A.Located String)], Src.RawExpr)
binopsHelp parseTerm parseLastTerm expr =
  choice
    [ commitIf (whitespace >> infixOpWith (fail "")) $
        do  whitespace
            op <- addLocation infixOp
            whitespace
            choice
              [ do  (ops, last) <- binopsHelp parseTerm parseLastTerm =<< try parseTerm
                    return ( (expr, op) : ops, last )
              , (,) [(expr, op)] <$> parseLastTerm
              ]
    , return ( [], expr )
    ]



-- INFIX OPERATORS


infixOp :: IParser String
infixOp =
  infixOpWith notReserved


infixOpWith :: IParser String -> IParser String
infixOpWith checker =
  expecting "an infix operator like (+)" $
    do  op <- checker <|> many1 (satisfy Help.isSymbol)
        case op of
          "." -> notFollowedBy lower >> return op
          _ -> return op


notReserved :: IParser a
notReserved =
  choice
    [ failOn "=" "The = operator is reserved for defining variables. Maybe you want == instead? Or maybe you are defining a variable, but there is whitespace before it?"
    , failOn "->" "Arrows are reserved for cases and anonymous functions. Maybe you want > or >= instead?"
    , failOn "|" "Vertical bars are reserved for use in union type declarations. Maybe you want || instead?"
    , failOn ":" "A single colon is for type annotations. Maybe you want :: instead? Or maybe you are defining a type annotation, but there is whitespace before it?"
    ]


failOn :: String -> String -> IParser a
failOn op msg =
  do  try $ lookAhead $ do
        string op
        notFollowedBy (satisfy Help.isSymbol)
      failure msg
