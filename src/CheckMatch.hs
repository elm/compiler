{-# LANGUAGE NamedFieldPuns #-}
module CheckMatch
    ( MatchResult (..)
    , ErrorType (..)
    , checkModule
    ) where

import Control.Applicative ((<$>))
import qualified AST.Expression.General as General
import qualified AST.Expression.Canonical as Canonical
import AST.Module
import qualified AST.Pattern as Pattern
import AST.Pattern (CanonicalPattern)
import qualified AST.Annotation as Annotation
import qualified Data.Map as Map
import qualified AST.Variable as Var
import qualified AST.Literal as Literal
import Data.Maybe (mapMaybe)
import Control.Monad (forM)
import qualified Data.Set as Set

data MatchResult
    = Redundant CanonicalPattern
    | Inexhaustive CanonicalPat
    | TypeNotFound
    | Ok

data Pat var
    = Data var [Pat var]
    | Record [String]
    | Alias String (Pat var)
    | Var String
    | Anything
    | Literal Literal.Literal
    -- It's a shame that I had to replicate the entire pattern type, but it's
    -- essential to have this constructor.
    | AnythingBut (Set.Set Literal.Literal)
    deriving (Show, Eq, Ord)

type CanonicalPat = Pat Var.Canonical

fromPattern :: CanonicalPattern -> CanonicalPat
fromPattern p = case p of
    Pattern.Data ctor ps -> Data ctor (map fromPattern ps)
    Pattern.Record fs    -> Record fs
    Pattern.Alias s p    -> Alias s (fromPattern p)
    Pattern.Var s        -> Var s
    Pattern.Anything     -> Anything
    Pattern.Literal l    -> Literal l

data ErrorType = ConstructorNotFound Var.Canonical

checkModule :: [CanonicalAdt] -> Module exports CanonicalBody -> [(Either ErrorType MatchResult, Annotation.Region)]
checkModule adts (Module {body}) = checkBody body where
    checkBody :: CanonicalBody -> [(Either ErrorType MatchResult, Annotation.Region)]
    checkBody  (CanonicalBody {program}) = checkExpr program

    checkExpr :: Canonical.Expr -> [(Either ErrorType MatchResult, Annotation.Region)]
    checkExpr (Annotation.A r e) = case e of
        General.ExplicitList es  -> concatMap checkExpr es
        General.Binop _ e e'     -> checkExpr e ++ checkExpr e'
        General.Lambda p e       -> (checkMatch [p], r) : checkExpr e
        General.App e e'         -> checkExpr e ++ checkExpr e'
        General.MultiIf branches -> concatMap (\(b,e) -> checkExpr b ++ checkExpr e) branches

        General.Let defs e -> ps ++ rhssResults ++ checkExpr e
            where
            ps          = map (\(Canonical.Definition p _ _) -> (checkMatch [p], r)) defs
            rhssResults = concatMap (\(Canonical.Definition _ e _) -> checkExpr e)  defs

        General.Case e branches ->
            (checkMatch ps, r) : checkExpr e ++ concatMap (checkExpr . snd) branches
            where ps = map fst branches

        General.Data _ctor es  -> concatMap checkExpr es
        General.Access e _f    -> checkExpr e
        General.Remove e _f    -> checkExpr e
        General.Insert e _f e' -> checkExpr e ++ checkExpr e'
        General.Modify e _     -> checkExpr e
        General.Record fields  -> concatMap (checkExpr . snd) fields
        General.Port _pi       -> []
        _                      -> []

    ctorToAdt :: Var.Canonical -> Maybe CanonicalAdt
    ctorToAdt = \k -> Map.lookup k m where
        m =
            Map.fromList $ concatMap (\adt@(_adtName, (_tyVars, ctors)) ->
                map (\(ctor,_args) -> (ctor, adt)) ctors)
                adts

    ctorToAdtExn :: Var.Canonical -> Either ErrorType CanonicalAdt
    ctorToAdtExn c = case ctorToAdt c of
        Nothing  -> Left (ConstructorNotFound c)
        Just adt -> Right adt

    complement :: CanonicalPat -> Either ErrorType [CanonicalPat]
    complement p = case p of
        Data ctor ps -> do
            (_adtName, (_tyVars, ctors)) <- ctorToAdtExn ctor
            -- A pattern is in the complement of this one if it uses one of the
            -- other constructors of this type...
            let otherCtorPatterns =
                    map (\(ctor',args) ->
                        Data ctor' (replicate (length args) Anything))
                        (filter (\(ctor', _) -> ctor' /= ctor) ctors)
            -- or if it uses this constructor and one of its argument patterns is
            -- in the complement of an argument pattern here.
                numArgs        = length ps
                sandwich pat i = replicate i Anything ++ pat : replicate (numArgs - i - 1) Anything

            argComplement <- fmap concat . forM (zip [0..] ps) $ \(i, argP) ->
                map (\argP' -> Data ctor (sandwich argP' i)) <$> (complement argP)

            return (otherCtorPatterns ++ argComplement)

        Record _fs     -> return []
        Alias _s p     -> complement p
        Var _s         -> return []
        Anything       -> return []
        Literal l      -> return [AnythingBut (Set.singleton l)]
        AnythingBut ls -> return (map Literal (Set.toList ls))

    intersection :: CanonicalPat -> CanonicalPat -> Maybe CanonicalPat
    intersection p1 p2 = case (p1, p2) of
        (Alias _ p1', _) -> intersection p1' p2
        (_, Alias _ p2') -> intersection p1 p2'
        (Anything, _)    -> Just p2
        (Var _, _)       -> Just p2
        (_, Anything)    -> Just p1
        (_, Var _)       -> Just p1

        (Data ctor1 args1, Data ctor2 args2) ->
            if ctor1 /= ctor2
            then Nothing
            -- TODO: Assumes that args must be the same length. I think that's ok.
            else fmap (Data ctor1) (sequence (zipWith intersection args1 args2))

        -- TODO Assuming the fields are identical.
        (Record _fs, Record _)             -> Just p1
        (Literal l1, Literal l2)           -> if l1 == l2 then Just p1 else Nothing
        (AnythingBut ls, Literal l)        -> if l `Set.member` ls then Nothing else Just p2
        (Literal l, AnythingBut ls)        -> if l `Set.member` ls then Nothing else Just p1
        (AnythingBut ls1, AnythingBut ls2) -> Just (AnythingBut (Set.union ls1 ls2))
        _                                  -> Nothing

    checkMatch :: [CanonicalPattern] -> Either ErrorType MatchResult
    checkMatch = go [Anything] where
        go :: [CanonicalPat] -> [CanonicalPattern] -> Either ErrorType MatchResult
        go [] []            = return Ok
        go (q:_) []         = return (Inexhaustive q)
        go unhandled (p:ps) =
            let pp = fromPattern p in
            if all (\q -> intersection q pp == Nothing) unhandled
            then return (Redundant p)
            else do
                pComp <- complement pp
                go (concatMap (\q -> mapMaybe (intersection q) pComp) unhandled) ps

