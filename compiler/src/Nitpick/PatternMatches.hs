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

import qualified Data.List as List hiding (foldr1)
import qualified Data.List.Utils as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE

import qualified Crash
import qualified Graph

import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified Elm.String as ES
import qualified Nitpick.Utils as Nit
import qualified Reporting.Annotation as A



-- PATTERN


data Pattern
  = Anything
  | Literal Literal
  | Ctor N.Name Tag.Tag Nit.Variants [Pattern]


data Literal
  = Chr Char
  | Str ES.String
  | Int Int
  deriving (Eq)



-- CREATE SIMPLIFIED PATTERNS


simplify :: Can.Pattern -> Pattern
simplify (A.At _ pattern) =
  case pattern of
    Can.PAnything           -> Anything
    Can.PVar _              -> Anything
    Can.PAlias p _          -> simplify p
    Can.PBool _ n t v       -> Ctor n t v []
    Can.PChr c              -> Literal (Chr c)
    Can.PStr s              -> Literal (Str s)
    Can.PInt n              -> Literal (Int n)
    Can.PUnit               -> Ctor N.unit   Tag.zero unit   []
    Can.PPair   a b         -> Ctor N.pair   Tag.zero pair   [ simplify a, simplify b ]
    Can.PTriple a b c       -> Ctor N.triple Tag.zero triple [ simplify a, simplify b, simplify c ]
    Can.PRecord _           -> Anything
    Can.PList ps            -> List.foldr (cons . simplify) nil ps
    Can.PCons p ps          -> cons (simplify p) (simplify ps)
    Can.PVariant n t tag xs -> Ctor n tag      (nits t) (List.map (simplify . Can._arg) xs)
    Can.PEnum    n t tag    -> Ctor n tag      (nits t) []
    Can.PVector  n t a b cs -> Ctor n Tag.zero (nits t) (List.map (simplify . Can._arg) (a:b:cs))
    Can.PBox     n t _ p    -> Ctor n Tag.zero (nits t) [simplify p]
    Can.PVoid    n t        -> Ctor n Tag.zero (nits t) []
  where
    nits = Can._p_variants



-- BUILT-IN UNIONS


{-# NOINLINE unit   #-}; unit   :: Nit.Variants; unit   = Nit.void N.unit
{-# NOINLINE pair   #-}; pair   :: Nit.Variants; pair   = Nit.vector 2 N.pair
{-# NOINLINE triple #-}; triple :: Nit.Variants; triple = Nit.vector 3 N.triple



-- LIST


cons :: Pattern -> Pattern -> Pattern
cons p ps =
  Ctor N.cons Tag.cons list [p,ps]


nil :: Pattern
nil =
  Ctor N.nil Tag.nil list []


{-# NOINLINE list #-}
list :: Nit.Variants
list =
  Nit.Variants (Nit.NumAlts 2)
    [ Nit.Variant N.cons Tag.cons 2
    , Nit.Variant N.nil  Tag.nil  0
    ]



-- ERROR


data Error
  = Incomplete A.Region Context [Pattern]
  | Redundant A.Region A.Region Int


data Context
  = BadArg
  | BadDestruct
  | BadCase



-- CHECK


check :: Tpd.Module -> Either (NE.List Error) ()
check (Tpd.Module _ _ _ decls _ _ _) =
  case checkDecls decls of
    []   -> Right ()
    e:es -> Left (NE.List e es)



-- CHECK DECLS


checkDecls :: Can.Decls -> [Error]
checkDecls (Can.Decls (Graph.Sorted decls)) =
    List.foldr checkDecl [] decls
  where
    checkDecl decl errors =
      case decl of
        Can.Decl    d    -> checkDef d errors
        Can.DeclRec d ds -> List.foldr1 checkDef errors d ds

    checkDef def errors =
      case def of
        Can.DValue_ _      e     -> checkExpr e errors
        Can.DValue  _      e _ _ -> checkExpr e errors
        Can.DFunc_  _ p ps e     -> List.foldr1 checkArg  (checkExpr e errors) p ps
        Can.DFunc   _ p ps e _ _ -> List.foldr1 checkPArg (checkExpr e errors) p ps



-- CHECK EXPRESSIONS


checkExpr :: Can.Expr -> [Error] -> [Error]
checkExpr (A.At region expression) errors =
  case expression of
    Can.VarLocal _          -> errors
    Can.VarTopLevel _ _     -> errors
    Can.VarKernel _ _       -> errors
    Can.VarForeign _ _ _    -> errors
    Can.VarDebug _ _ _      -> errors
    Can.VarOperator _ _ _ _ -> errors
    Can.CRecord _ _ _ _ _   -> errors
    Can.CVariant _ _ _ _ _  -> errors
    Can.CEnum _ _ _ _       -> errors
    Can.CVector _ _ _ _     -> errors
    Can.CBox _ _ _          -> errors
    Can.CVoid _ _ _         -> errors
    Can.Bool _              -> errors
    Can.Chr _               -> errors
    Can.Str _               -> errors
    Can.Int _               -> errors
    Can.Float _             -> errors
    Can.List es             -> foldr checkExpr errors es
    Can.Negate e            -> checkExpr e errors
    Can.Binop _ _ _ _ x y   -> checkExpr x $ checkExpr y errors
    Can.Lambda x xs e       -> checkArgs x xs (checkExpr e errors)
    Can.Call f e es         -> checkExpr f $ List.foldr1 checkExpr errors e es
    Can.If bs f             -> foldr checkIfBranch (checkExpr f errors) bs
    Can.LetValue v e        -> checkValue v $ checkExpr e errors
    Can.LetFunc f e         -> checkFunc f $ checkExpr e errors
    Can.LetRec f fs e       -> List.foldr1 checkFunc (checkExpr e errors) f fs
    Can.LetDestruct p e b   -> checkPatterns (A.toRegion p) BadDestruct [p] $ checkExpr e $ checkExpr b errors
    Can.Case e b bs         -> checkExpr e $ checkCases region b bs errors
    Can.Unit                -> errors
    Can.Pair   a b          -> checkExpr a $ checkExpr b errors
    Can.Triple a b c        -> checkExpr a $ checkExpr b $ checkExpr c errors
    Can.Empty               -> errors
    Can.Record fs           -> F.foldr checkExpr errors fs
    Can.Update _ e fs       -> checkExpr e $ F.foldr checkUpdate errors fs
    Can.Access e _          -> checkExpr e errors
    Can.Accessor _          -> errors
    Can.Shader _ _          -> errors


checkUpdate :: Can.Update -> [Error] -> [Error]
checkUpdate (Can.Update_ _ expr) errors =
  checkExpr expr errors



-- CHECK IF BRANCH


checkIfBranch :: (Can.Expr, Can.Expr) -> [Error] -> [Error]
checkIfBranch (condition, branch) errs =
  checkExpr condition $ checkExpr branch errs



-- CHECK CASE EXPRESSION


checkCases :: A.Region -> Can.Branch -> [Can.Branch] -> [Error] -> [Error]
checkCases region branch branches errors =
  let
    (patterns, newErrors) =
      List.foldr1 checkCaseBranch ([], errors) branch branches
  in
  checkPatterns region BadCase patterns newErrors


checkCaseBranch :: Can.Branch -> ([Can.Pattern], [Error]) -> ([Can.Pattern], [Error])
checkCaseBranch (Can.Branch pattern expr) (patterns, errors) =
  ( pattern:patterns
  , checkExpr expr errors
  )



-- CHECK LET


checkValue :: Can.Value -> [Error] -> [Error]
checkValue value errors =
  case value of
    Can.Value_ _ e     -> checkExpr e errors
    Can.Value  _ e _ _ -> checkExpr e errors


checkFunc :: Can.Func -> [Error] -> [Error]
checkFunc func errors =
  case func of
    Can.Func_ _ x xs e     -> List.foldr1 checkArg  (checkExpr e errors) x xs
    Can.Func  _ x xs e _ _ -> List.foldr1 checkPArg (checkExpr e errors) x xs



-- CHECK ARGS


checkArgs :: Can.Pattern -> [Can.Pattern] -> [Error] -> [Error]
checkArgs p ps errors =
  List.foldr1 checkArg errors p ps


checkArg :: Can.Pattern -> [Error] -> [Error]
checkArg pattern@(A.At region _) errors =
  checkPatterns region BadArg [pattern] errors


checkPArg :: Can.PArg -> [Error] -> [Error]
checkPArg (Can.PArg _ _ pattern@(A.At region _)) errors =
  checkPatterns region BadArg [pattern] errors



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
        let variants@(Nit.Variants_ m vs) = snd (Map.findMin ctors) in
        if numSeen < m then
          (:)
            <$> Maybe.mapMaybe (isMissing variants ctors) vs
            <*> isExhaustive (Maybe.mapMaybe specializeRowByAnything matrix) (n - 1)

        else
          let
            isAltExhaustive (Nit.Variant name tag arity) =
              recoverCtor variants name tag arity <$>
              isExhaustive
                (Maybe.mapMaybe (specializeRowByCtor tag arity) matrix)
                (arity + n - 1)
          in
          concatMap isAltExhaustive vs


isMissing :: Nit.Variants -> Map.Map Tag.Tag a -> Nit.Variant -> Maybe Pattern
isMissing variants ctors (Nit.Variant name tag arity) =
  if Map.member tag ctors then
    Nothing
  else
    Just (Ctor name tag variants (replicate arity Anything))


recoverCtor :: Nit.Variants -> N.Name -> Tag.Tag -> Int -> [Pattern] -> [Pattern]
recoverCtor variants name tag arity patterns =
  let
    (args, rest) =
      splitAt arity patterns
  in
  Ctor name tag variants args : rest



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
            Ctor _ tag _ args ->
              -- keep checking rows that start with this Ctor or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByCtor tag (length args)) matrix)
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
                    isUsefulAlt (Nit.Variant _ tag arity) =
                      isUseful
                        (Maybe.mapMaybe (specializeRowByCtor tag arity) matrix)
                        (replicate arity Anything ++ patterns)
                  in
                    any isUsefulAlt alts

            Literal literal ->
              -- keep checking rows that start with this Literal or Anything
              isUseful
                (Maybe.mapMaybe (specializeRowByLiteral literal) matrix)
                patterns


-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)
specializeRowByCtor :: Tag.Tag -> Int -> [Pattern] -> Maybe [Pattern]
specializeRowByCtor ctorTag arity row =
  case row of
    Ctor _ tag _ args : patterns ->
      if tag == ctorTag then
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

    Ctor _ _ _ _ : _ ->
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
    Ctor _ _ _ _ : _    -> Nothing
    Anything : patterns -> Just patterns
    Literal _ : _       -> Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


data Complete
  = Yes [Nit.Variant]
  | No


isComplete :: [[Pattern]] -> Complete
isComplete matrix =
  let
    ctors = collectCtors matrix
    numSeen = Map.size ctors
  in
    if numSeen == 0
    then No
    else
      let
        (Nit.Variants_ n vs) = snd (Map.findMin ctors)
      in
      if numSeen == n
      then Yes vs
      else No



-- COLLECT CTORS


collectCtors :: [[Pattern]] -> Map.Map Tag.Tag Nit.Variants
collectCtors matrix =
  List.foldl' collectCtorsHelp Map.empty matrix


collectCtorsHelp :: Map.Map Tag.Tag Nit.Variants -> [Pattern] -> Map.Map Tag.Tag Nit.Variants
collectCtorsHelp ctors row =
  case row of
    Ctor _ tag variants _ : _ -> Map.insert tag variants ctors
    _                          -> ctors
