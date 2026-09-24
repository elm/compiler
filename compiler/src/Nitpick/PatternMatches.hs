{-# OPTIONS_GHC -fno-warn-x-partial #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Nitpick.PatternMatches
  ( check
  , Error(..)
  , Context(..)
  , Pattern(..)
  , Literal(..)
  )
  where


{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Reporting.Annotation as A



-- PATTERN


data Pattern
  = Anything
  | Literal Literal
  | Ctor Can.Union N.Name [Pattern]


data Literal
  = Chr Char
  | Str ES.String
  | Int Int
  deriving (Eq)



-- CREATE SIMPLIFIED PATTERNS


simplify :: Can.Pattern -> Pattern
simplify (A.At _ pattern) =
  case pattern of
    Can.PAnything          -> Anything
    Can.PVar _             -> Anything
    Can.PRecord _          -> Anything
    Can.PInt n             -> Literal (Int n)
    Can.PStr s             -> Literal (Str s)
    Can.PChr c             -> Literal (Chr c)
    Can.PBool u b          -> Ctor u (if b then N.true else N.false) []
    Can.PUnit              -> Ctor unit N.unit []
    Can.PList ps           -> foldr cons nil ps
    Can.PCons p ps         -> cons p (simplify ps)
    Can.PAlias p _         -> simplify p
    Can.PCtor _ _ u n _ ps -> Ctor u n $ map (\(Can.PatternCtorArg _ _ arg) -> simplify arg) ps
    Can.PTuple a b mc      ->
      case mc of
        Nothing -> Ctor pair   N.pair   [ simplify a, simplify b ]
        Just c  -> Ctor triple N.triple [ simplify a, simplify b, simplify c ]


cons :: Can.Pattern -> Pattern -> Pattern
cons hd tl =
  Ctor list N.cons [ simplify hd, tl ]


{-# NOINLINE nil #-}
nil :: Pattern
nil =
  Ctor list N.nil []



-- BUILT-IN UNIONS


{-# NOINLINE unit #-}
unit :: Can.Union
unit =
  let
    ctor = Can.Ctor N.unit Index.first 0 []
  in
  Can.Union [] [ ctor ] 1 Can.Normal


{-# NOINLINE pair #-}
pair :: Can.Union
pair =
    Can.Union [T.a,T.b] [ ctor ] 1 Can.Normal
  where
    ctor = Can.Ctor N.pair Index.first 2 [Can.TVar T.a, Can.TVar T.b]


{-# NOINLINE triple #-}
triple :: Can.Union
triple =
  let
    ctor = Can.Ctor N.triple Index.first 3 [Can.TVar T.a, Can.TVar T.b, Can.TVar T.c]
  in
  Can.Union [T.a,T.b,T.c] [ ctor ] 1 Can.Normal


{-# NOINLINE list #-}
list :: Can.Union
list =
  let
    nilCtor =
      Can.Ctor N.nil Index.first 0 []

    consCtor =
      Can.Ctor N.cons Index.second 2
        [ Can.TVar T.a
        , Can.TType ModuleName.list T.list [Can.TVar T.a]
        ]
  in
  Can.Union [T.a] [ nilCtor, consCtor ] 2 Can.Normal



-- ERROR


data Error
  = Incomplete A.Region Context [Pattern]
  | Redundant A.Region A.Region Int


data Context
  = BadArg
  | BadDestruct
  | BadCase



-- CHECK


check :: Can.Module -> Either (NE.List Error) ()
check (Can.Module _ _ _ decls _ _ _ _) =
  case checkDecls decls [] of
    []   -> Right ()
    e:es -> Left (NE.List e es)



-- CHECK DECLS


checkDecls :: Can.Decls -> [Error] -> [Error]
checkDecls decls errors =
  case decls of
    Can.Declare def subDecls ->
      checkDef def $ checkDecls subDecls errors

    Can.DeclareRec def defs subDecls ->
      checkDef def (foldr checkDef (checkDecls subDecls errors) defs)

    Can.SaveTheEnvironment ->
      errors



-- CHECK DEFS


checkDef :: Can.Def -> [Error] -> [Error]
checkDef def errors =
  case def of
    Can.Def      _   xs e   -> foldr checkArg      (checkExpr e errors) xs
    Can.TypedDef _ _ xs e _ -> foldr checkTypedArg (checkExpr e errors) xs


checkArg :: Can.Pattern -> [Error] -> [Error]
checkArg pattern@(A.At region _) errors =
  checkPatterns region BadArg [pattern] errors


checkTypedArg :: (Can.Pattern, tipe) -> [Error] -> [Error]
checkTypedArg (pattern@(A.At region _), _) errors =
  checkPatterns region BadArg [pattern] errors



-- CHECK EXPRESSIONS


checkExpr :: Can.Expr -> [Error] -> [Error]
checkExpr (A.At region expression) errors =
  case expression of
    Can.VarLocal _          -> errors
    Can.VarTopLevel _ _     -> errors
    Can.VarKernel _ _       -> errors
    Can.VarForeign _ _ _    -> errors
    Can.VarCtor _ _ _ _ _   -> errors
    Can.VarDebug _ _ _      -> errors
    Can.VarOperator _ _ _ _ -> errors
    Can.Chr _               -> errors
    Can.Str _               -> errors
    Can.Int _               -> errors
    Can.Float _             -> errors
    Can.List es             -> foldr checkExpr errors es
    Can.Negate e            -> checkExpr e errors
    Can.Binop _ _ _ _ x y   -> checkExpr x $ checkExpr y errors
    Can.Lambda xs e         -> foldr checkArg (checkExpr e errors) xs
    Can.Call f xs           -> checkExpr f $ foldr checkExpr errors xs
    Can.If bs f             -> foldr checkIfBranch (checkExpr f errors) bs
    Can.Let d e             -> checkDef d $ checkExpr e errors
    Can.LetRec ds e         -> foldr checkDef (checkExpr e errors) ds
    Can.LetDestruct p e b   -> checkPatterns (A.toRegion p) BadDestruct [p] $ checkExpr e $ checkExpr b errors
    Can.Case e bs           -> checkExpr e $ checkCases region bs errors
    Can.Accessor _          -> errors
    Can.Access e _          -> checkExpr e errors
    Can.Update _ e fs       -> checkExpr e $ Map.foldr checkField errors fs
    Can.Record fs           -> Map.foldr checkExpr errors fs
    Can.Unit                -> errors
    Can.Tuple a b Nothing   -> checkExpr a $ checkExpr b errors
    Can.Tuple a b (Just c)  -> checkExpr a $ checkExpr b $ checkExpr c errors
    Can.Shader _ _          -> errors



-- CHECK FIELD


checkField :: Can.FieldUpdate -> [Error] -> [Error]
checkField (Can.FieldUpdate _ expr) errors =
  checkExpr expr errors



-- CHECK IF BRANCH


checkIfBranch :: (Can.Expr, Can.Expr) -> [Error] -> [Error]
checkIfBranch (condition, branch) errs =
  checkExpr condition $ checkExpr branch errs



-- CHECK CASE EXPRESSION


checkCases :: A.Region -> [Can.CaseBranch] -> [Error] -> [Error]
checkCases region branches errors =
  let
    (patterns, newErrors) =
      foldr checkCaseBranch ([], errors) branches
  in
  checkPatterns region BadCase patterns newErrors


checkCaseBranch :: Can.CaseBranch -> ([Can.Pattern], [Error]) -> ([Can.Pattern], [Error])
checkCaseBranch (Can.CaseBranch pattern expr) (patterns, errors) =
  ( pattern:patterns
  , checkExpr expr errors
  )



-- CHECK PATTERNS


checkPatterns :: A.Region -> Context -> [Can.Pattern] -> [Error] -> [Error]
checkPatterns region context patterns errors =
  case toNonRedundantRows region patterns of
    Left err ->
      err:errors

    Right matrix ->
      case isExhaustive matrix 1 of
        [] ->
          errors

        badPatterns ->
          Incomplete region context (map head badPatterns) : errors



-- EXHAUSTIVE PATTERNS


-- INVARIANTS:
--
--   The initial rows "matrix" are all of length 1
--   The initial count of items per row "n" is also 1
--   The resulting rows are examples of missing patterns
--
isExhaustive :: [[Pattern]] -> Int -> [[Pattern]]
isExhaustive matrix n =
  case matrix of
    [] ->
      [replicate n Anything]

    _ ->
      if n == 0 then
        []
      else
      let
        ctors = collectCtors matrix
        numSeen = Map.size ctors
      in
      if numSeen == 0 then
        (:) Anything
          <$> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

      else
        let alts@(Can.Union _ altList numAlts _) = snd (Map.findMin ctors) in
        if numSeen < numAlts then
          (:)
            <$> Maybe.mapMaybe (isMissing alts ctors) altList
            <*> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

        else
          let
            isAltExhaustive (Can.Ctor name _ arity _) =
              recoverCtor alts name arity <$>
              isExhaustive
                (Maybe.mapMaybe (specializeRowByCtor name arity) matrix)
                (arity + n - 1)
          in
          concatMap isAltExhaustive altList


isMissing :: Can.Union -> Map.Map N.Name a -> Can.Ctor -> Maybe Pattern
isMissing union ctors (Can.Ctor name _ arity _) =
  if Map.member name ctors then
    Nothing
  else
    Just (Ctor union name (replicate arity Anything))


recoverCtor :: Can.Union -> N.Name -> Int -> [Pattern] -> [Pattern]
recoverCtor union name arity patterns =
  let
    (args, rest) =
      splitAt arity patterns
  in
  Ctor union name args : rest



-- REDUNDANT PATTERNS


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toNonRedundantRows :: A.Region -> [Can.Pattern] -> Either Error [[Pattern]]
toNonRedundantRows region patterns =
  toSimplifiedUsefulRows region [] patterns


-- INVARIANT: Produces a list of rows where (forall row. length row == 1)
toSimplifiedUsefulRows :: A.Region -> [[Pattern]] -> [Can.Pattern] -> Either Error [[Pattern]]
toSimplifiedUsefulRows overallRegion checkedRows uncheckedPatterns =
  case uncheckedPatterns of
    [] ->
      Right checkedRows

    pattern@(A.At region _) : rest ->
      let nextRow = [simplify pattern] in
      if isUseful checkedRows nextRow then
        toSimplifiedUsefulRows overallRegion (nextRow : checkedRows) rest
      else
        Left (Redundant overallRegion region (length checkedRows + 1))


-- Check if a new row "vector" is useful given previous rows "matrix"
isUseful :: [[Pattern]] -> [Pattern] -> Bool
isUseful matrix vector =
  case matrix of
    [] ->
      -- No rows are the same as the new vector! The vector is useful!
      True

    _ ->
      case vector of
        [] ->
          -- There is nothing left in the new vector, but we still have
          -- rows that match the same things. This is not a useful vector!
          False

        firstPattern : patterns ->
          case firstPattern of
            Ctor _ name args ->
              -- keep checking rows that start with this Ctor or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByCtor name (length args)) matrix)
                (args ++ patterns)

            Anything ->
              -- check if all alts appear in matrix
              case isComplete matrix of
                No ->
                  -- This Anything is useful because some Ctors are missing.
                  -- But what if a previous row has an Anything?
                  -- If so, this one is not useful.
                  isUseful (Maybe.mapMaybe specializeRowByAnything matrix) patterns

                Yes alts ->
                  -- All Ctors are covered, so this Anything is not needed for any
                  -- of those. But what if some of those Ctors have subpatterns
                  -- that make them less general? If so, this actually is useful!
                  let
                    isUsefulAlt (Can.Ctor name _ arity _) =
                      isUseful
                        (Maybe.mapMaybe (specializeRowByCtor name arity) matrix)
                        (replicate arity Anything ++ patterns)
                  in
                    any isUsefulAlt alts

            Literal literal ->
              -- keep checking rows that start with this Literal or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByLiteral literal) matrix)
                patterns


-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByCtor :: N.Name -> Int -> [Pattern] -> Maybe [Pattern]
specializeRowByCtor ctorName arity row =
  case row of
    Ctor _ name args : patterns ->
      if name == ctorName then
        Just (args ++ patterns)
      else
        Nothing

    Anything : patterns ->
      Just (replicate arity Anything ++ patterns)

    Literal _ : _ ->
      $(Crash.crash 'specializeRowByCtor) $
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      $(Crash.crash 'specializeRowByCtor) "Compiler error! Empty matrices should not get specialized."


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByLiteral :: Literal -> [Pattern] -> Maybe [Pattern]
specializeRowByLiteral literal row =
  case row of
    Literal lit : patterns ->
      if lit == literal then
        Just patterns
      else
        Nothing

    Anything : patterns ->
      Just patterns

    Ctor _ _ _ : _ ->
      $(Crash.crash 'specializeRowByLiteral) $
        "Compiler bug! After type checking, constructors and literals\
        \ should never align in pattern match exhaustiveness checks."

    [] ->
      $(Crash.crash 'specializeRowByLiteral) "Compiler error! Empty matrices should not get specialized."


-- INVARIANT: (length row == N) ==> (length result == N-1)
specializeRowByAnything :: [Pattern] -> Maybe [Pattern]
specializeRowByAnything row =
  case row of
    []                  -> Nothing
    Ctor _ _ _ : _      -> Nothing
    Anything : patterns -> Just patterns
    Literal _ : _       -> Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


data Complete
  = Yes [Can.Ctor]
  | No


isComplete :: [[Pattern]] -> Complete
isComplete matrix =
  let
    ctors = collectCtors matrix
    numSeen = Map.size ctors
  in
    if numSeen == 0 then
      No
    else
      let (Can.Union _ alts numAlts _) = snd (Map.findMin ctors) in
      if numSeen == numAlts then Yes alts else No



-- COLLECT CTORS


collectCtors :: [[Pattern]] -> Map.Map N.Name Can.Union
collectCtors matrix =
  List.foldl' collectCtorsHelp Map.empty matrix


collectCtorsHelp :: Map.Map N.Name Can.Union -> [Pattern] -> Map.Map N.Name Can.Union
collectCtorsHelp ctors row =
  case row of
    Ctor union name _ : _ -> Map.insert name union ctors
    _                     -> ctors
