{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Pattern
  ( verify
  , Bindings
  , canonicalize
  , canonicalizeArg
  )
  where


import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Data.Index as Index
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULTS


type Result i =
  Result.Result i Warning.Warning Error.Error



-- VERIFY NAMES


verify :: Error.DuplicatePatternContext -> Result Bindings a -> Result () (a, Can.Destructors)
verify context (Result.Result bindings warnings answer) =
  case answer of
    Result.Ok value ->
      do  let toError name () () = Error.DuplicatePattern context name
          destructors <- Dups.detect toError bindings
          Result.Result () warnings $ Result.Ok (value, destructors)

    Result.Err err ->
      Result.Result () warnings (Result.Err err)



-- CANONICALIZE ARG


canonicalizeArg :: Env.Env -> Index.ZeroBased -> Src.Pattern -> Result Bindings Can.Arg
canonicalizeArg env index pattern =
  Can.Arg index <$> canonicalize env index pattern



-- CANONICALIZE


type Bindings =
  Dups.Dict () (A.Located Can.Destructor)


canonicalize :: Env.Env -> Index.ZeroBased -> Src.Pattern -> Result Bindings Can.Pattern
canonicalize env index pattern =
  process env (Can.DRoot index) pattern


process :: Env.Env -> Can.Destructor -> Src.Pattern -> Result Bindings Can.Pattern
process env destructor (A.At region pattern) =
  A.At region <$>
  case pattern of
    Src.PAnything ->
      Result.ok Can.PAnything

    Src.PVar name ->
      Result.accumulate
        (Dups.one name region () (A.At region destructor))
        (Can.PVar name)

    Src.PRecord fields ->
      let
        addField (A.At reg name) dict =
          Dups.insert name reg () (A.At reg (Can.DField name destructor)) dict
      in
      Result.accumulate
        (foldr addField Dups.none fields)
        (Can.PRecord (map A.drop fields))

    Src.PUnit ->
      Result.ok Can.PUnit

    Src.PTuple a b cs ->
      Can.PTuple
        <$> process env (Can.DIndex Index.first destructor) a
        <*> process env (Can.DIndex Index.second destructor) b
        <*> processTuple region env destructor cs

    Src.PCtor nameRegion maybePrefix name patterns ->
      let
        toCanonicalArg index ptrn tipe =
          Can.PatternCtorArg index tipe
            <$> process env (Can.DIndex index destructor) ptrn
      in
      do  (Env.Pattern home tipe vars alts args) <- Env.findPattern nameRegion env maybePrefix name
          verifiedList <- Index.indexedZipWithA toCanonicalArg patterns args
          case verifiedList of
            Index.LengthMatch cargs ->
              Result.ok $ Can.PCtor home tipe vars alts name cargs

            Index.LengthMismatch actualLength expectedLength ->
              Result.throw region (error "TODO" actualLength expectedLength)

    Src.PList patterns ->
      Can.PList <$> processList env destructor patterns

    Src.PCons first rest ->
      Can.PCons
        <$> process env (Can.DIndex Index.first destructor) first
        <*> process env (Can.DIndex Index.second destructor) rest

    Src.PAlias ptrn (A.At reg name) ->
      do  cpattern <- process env destructor ptrn
          Result.accumulate
            (Dups.one name reg () (A.At reg destructor))
            (Can.PAlias cpattern name)

    Src.PChr chr ->
      Result.ok (Can.PChr chr)

    Src.PStr str ->
      Result.ok (Can.PStr str)

    Src.PInt int ->
      Result.ok (Can.PInt int)


processTuple :: R.Region -> Env.Env -> Can.Destructor -> [Src.Pattern] -> Result Bindings (Maybe Can.Pattern)
processTuple tupleRegion env destructor extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> process env (Can.DIndex Index.third destructor) three

    _ : others ->
      let (A.At r1 _, A.At r2 _) = (head others, last others) in
      Result.throw tupleRegion (Error.TupleLargerThanThree (R.merge r1 r2))


processList :: Env.Env -> Can.Destructor -> [Src.Pattern] -> Result Bindings [Can.Pattern]
processList env destructor list =
  case list of
    [] ->
      Result.ok []

    pattern : otherPatterns ->
      (:)
        <$> process env (Can.DIndex Index.first destructor) pattern
        <*> processList env (Can.DIndex Index.second destructor) otherPatterns
