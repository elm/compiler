module Parse.Binop (infixOp, binops, OpTable) where

import qualified Data.List as List
import qualified Data.Map as Map
import Text.Parsec
  ( (<|>), choice, getState, lookAhead, lower, many1
  , notFollowedBy, satisfy, string, try
  )

import AST.Declaration (Assoc(L, N, R))
import AST.Expression.General (Expr'(Binop))
import qualified AST.Expression.Source as Source
import qualified AST.Helpers as Help
import qualified AST.Variable as Var
import Parse.Helpers (IParser, OpTable, commitIf, expecting, failure, whitespace)
import qualified Reporting.Annotation as A



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


failOn :: String -> String -> IParser a
failOn op msg =
  do  try $ lookAhead $ do
        string op
        notFollowedBy (satisfy Help.isSymbol)
      failure msg


notReserved :: IParser a
notReserved =
  choice
    [ failOn "=" "The = operator is reserved for defining variables. Maybe you want == instead? Or maybe you are defining a variable, but there is whitespace before it?"
    , failOn "->" "Arrows are reserved for cases and anonymous functions. Maybe you want > or >= instead?"
    , failOn "|" "Vertical bars are reserved for use in union type declarations. Maybe you want || instead?"
    , failOn ":" "A single colon is for type annotations. Maybe you want :: instead? Or maybe you are defining a type annotation, but there is whitespace before it?"
    ]



-- BINARY EXPRESSIONS


opLevel :: OpTable -> String -> Int
opLevel table op =
  fst $ Map.findWithDefault (9,L) op table


opAssoc :: OpTable -> String -> Assoc
opAssoc table op =
  snd $ Map.findWithDefault (9,L) op table


hasLevel :: OpTable -> Int -> (String, Source.Expr) -> Bool
hasLevel table n (op,_) =
  opLevel table op == n


binops
    :: IParser Source.Expr
    -> IParser Source.Expr
    -> IParser Source.Expr
binops term last =
  do  e <- term
      table <- getState
      split table 0 e =<< nextOps
  where
    nextOps =
      choice
        [ commitIf (whitespace >> infixOpWith (fail "")) $
            do  whitespace
                op <- infixOp
                whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (op,t) <$> nextOps
                  Right e -> return [(op,e)]
        , return []
        ]


split
    :: OpTable
    -> Int
    -> Source.Expr
    -> [(String, Source.Expr)]
    -> IParser Source.Expr
split _ _ e [] = return e
split table n e eops =
  do  assoc <- getAssoc table n eops
      es <- sequence (splitLevel table n e eops)
      let ops = map fst (filter (hasLevel table n) eops)
      case assoc of
        R -> joinR es ops
        _ -> joinL es ops


splitLevel
    :: OpTable
    -> Int
    -> Source.Expr
    -> [(String, Source.Expr)]
    -> [IParser Source.Expr]
splitLevel table n e eops =
  case break (hasLevel table n) eops of
    (lops, (_op,e'):rops) ->
        split table (n+1) e lops : splitLevel table n e' rops

    (lops, []) ->
        [ split table (n+1) e lops ]


joinL :: [Source.Expr] -> [String] -> IParser Source.Expr
joinL exprs ops =
  case (exprs, ops) of
    ([expr], []) ->
        return expr

    (a:b:remainingExprs, op:remainingOps) ->
        let binop = A.merge a b (Binop (Var.Raw op) a b)
        in
            joinL (binop : remainingExprs) remainingOps

    (_, _) ->
        failure "Ill-formed binary expression. Report a compiler bug."


joinR :: [Source.Expr] -> [String] -> IParser Source.Expr
joinR exprs ops =
  case (exprs, ops) of
    ([expr], []) ->
        return expr

    (a:b:remainingExprs, op:remainingOps) ->
        do  e <- joinR (b:remainingExprs) remainingOps
            return (A.merge a e (Binop (Var.Raw op) a e))

    (_, _) ->
        failure "Ill-formed binary expression. Report a compiler bug."


getAssoc :: OpTable -> Int -> [(String,Source.Expr)] -> IParser Assoc
getAssoc table n eops
    | all (==L) assocs = return L
    | all (==R) assocs = return R
    | all (==N) assocs =
        case assocs of
          [_] -> return N
          _   -> failure (msg "precedence")
    | otherwise = failure (msg "associativity")
  where
    levelOps = filter (hasLevel table n) eops
    assocs = map (opAssoc table . fst) levelOps
    msg problem =
        concat
          [ "Conflicting " ++ problem ++ " for binary operators ("
          , List.intercalate ", " (map fst eops), "). "
          , "Consider adding parentheses to disambiguate."
          ]
