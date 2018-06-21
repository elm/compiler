{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Pattern
  ( verify
  , Bindings
  , DupsDict
  , canonicalize
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- RESULTS


type Result i w a =
  Result.Result i w Error.Error a


type Bindings =
  Map.Map N.Name R.Region



-- VERIFY


verify :: Error.DuplicatePatternContext -> Result DupsDict w a -> Result i w (a, Bindings)
verify context (Result.Result k) =
  Result.Result $ \info warnings bad good ->
    k Dups.none warnings
      (\_ warnings1 errors ->
          bad info warnings1 errors
      )
      (\bindings warnings1 value ->
          case Dups.detect (Error.DuplicatePattern context) bindings of
            Result.Result k1 ->
              k1 () ()
                (\() () errs -> bad info warnings1 errs)
                (\() () dict -> good info warnings1 (value, dict))
      )



-- CANONICALIZE


type DupsDict =
  Dups.Dict R.Region


canonicalize :: Env.Env -> Src.Pattern -> Result DupsDict w Can.Pattern
canonicalize env (A.At region pattern) =
  A.At region <$>
  case pattern of
    Src.PAnything ->
      Result.ok Can.PAnything

    Src.PVar name ->
      logVar name region (Can.PVar name)

    Src.PRecord fields ->
      logFields fields (Can.PRecord (map A.toValue fields))

    Src.PUnit ->
      Result.ok Can.PUnit

    Src.PTuple a b cs ->
      Can.PTuple
        <$> canonicalize env a
        <*> canonicalize env b
        <*> canonicalizeTuple region env cs

    Src.PCtor nameRegion name patterns ->
      canonicalizeCtor env region name patterns =<< Env.findCtor nameRegion env name

    Src.PCtorQual nameRegion home name patterns ->
      canonicalizeCtor env region name patterns =<< Env.findCtorQual nameRegion env home name

    Src.PList patterns ->
      Can.PList <$> canonicalizeList env patterns

    Src.PCons first rest ->
      Can.PCons
        <$> canonicalize env first
        <*> canonicalize env rest

    Src.PAlias ptrn (A.At reg name) ->
      do  cpattern <- canonicalize env ptrn
          logVar name reg (Can.PAlias cpattern name)

    Src.PChr chr ->
      Result.ok (Can.PChr chr)

    Src.PStr str ->
      Result.ok (Can.PStr str)

    Src.PInt int ->
      Result.ok (Can.PInt int)


canonicalizeCtor :: Env.Env -> R.Region -> N.Name -> [Src.Pattern] -> Env.Ctor -> Result DupsDict w Can.Pattern_
canonicalizeCtor env region name patterns ctor =
  case ctor of
    Env.Ctor home tipe union index args ->
      let
        toCanonicalArg argIndex argPattern argTipe =
          Can.PatternCtorArg argIndex argTipe <$> canonicalize env argPattern
      in
      do  verifiedList <- Index.indexedZipWithA toCanonicalArg patterns args
          case verifiedList of
            Index.LengthMatch cargs ->
              if tipe == N.bool && home == ModuleName.basics then
                Result.ok (Can.PBool union (name == N.true))
              else
                Result.ok (Can.PCtor home tipe union name index cargs)

            Index.LengthMismatch actualLength expectedLength ->
              Result.throw (Error.BadArity region Error.PatternArity name expectedLength actualLength)

    Env.RecordCtor _ _ _ ->
      Result.throw (Error.PatternHasRecordCtor region name)


canonicalizeTuple :: R.Region -> Env.Env -> [Src.Pattern] -> Result DupsDict w (Maybe Can.Pattern)
canonicalizeTuple tupleRegion env extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> canonicalize env three

    _ ->
      Result.throw $ Error.TupleLargerThanThree tupleRegion


canonicalizeList :: Env.Env -> [Src.Pattern] -> Result DupsDict w [Can.Pattern]
canonicalizeList env list =
  case list of
    [] ->
      Result.ok []

    pattern : otherPatterns ->
      (:)
        <$> canonicalize env pattern
        <*> canonicalizeList env otherPatterns



-- LOG BINDINGS


logVar :: N.Name -> R.Region -> a -> Result DupsDict w a
logVar name region value =
  Result.Result $ \bindings warnings _ ok ->
    ok (Dups.insert name region region bindings) warnings value


logFields :: [A.Located N.Name] -> a -> Result DupsDict w a
logFields fields value =
  let
    addField (A.At region name) dict =
      Dups.insert name region region dict
  in
  Result.Result $ \bindings warnings _ ok ->
    ok (foldr addField bindings fields) warnings value
