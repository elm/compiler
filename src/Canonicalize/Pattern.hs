{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Pattern
  ( verify
  , Bindings
  , DupsDict
  , canonicalize
  , canonicalizeArg
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Result as Result
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R



-- RESULTS


type Result i w a =
  Result.Result i w Error.Error a


type Bindings =
  Map.Map N.Name (A.Located Can.Destructor)



-- VERIFY NAMES


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
  Dups.Dict () (A.Located Can.Destructor)


canonicalizeArg :: Env.Env -> Index.ZeroBased -> Src.Pattern -> Result DupsDict w Can.Arg
canonicalizeArg env index pattern =
  Can.Arg index <$> canonicalize env index pattern


canonicalize :: Env.Env -> Index.ZeroBased -> Src.Pattern -> Result DupsDict w Can.Pattern
canonicalize env index pattern =
  process env (Can.DRoot index) pattern



-- PROCESS PATTERNS


process :: Env.Env -> Can.Destructor -> Src.Pattern -> Result DupsDict w Can.Pattern
process env destructor (A.At region pattern) =
  A.At region <$>
  case pattern of
    Src.PAnything ->
      Result.ok Can.PAnything

    Src.PVar name ->
      logVar name region destructor (Can.PVar name)

    Src.PRecord fields ->
      logFields fields destructor (Can.PRecord (map A.drop fields))

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
              Result.ok (Can.PCtor home tipe vars alts name cargs)

            Index.LengthMismatch actualLength expectedLength ->
              Result.throw (error "TODO" region actualLength expectedLength)

    Src.PList patterns ->
      Can.PList <$> processList env destructor patterns

    Src.PCons first rest ->
      Can.PCons
        <$> process env (Can.DIndex Index.first destructor) first
        <*> process env (Can.DIndex Index.second destructor) rest

    Src.PAlias ptrn (A.At reg name) ->
      do  cpattern <- process env destructor ptrn
          logVar name reg destructor (Can.PAlias cpattern name)

    Src.PChr chr ->
      Result.ok (Can.PChr chr)

    Src.PStr str ->
      Result.ok (Can.PStr str)

    Src.PInt int ->
      Result.ok (Can.PInt int)


processTuple :: R.Region -> Env.Env -> Can.Destructor -> [Src.Pattern] -> Result DupsDict w (Maybe Can.Pattern)
processTuple tupleRegion env destructor extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> process env (Can.DIndex Index.third destructor) three

    _ : others ->
      let (A.At r1 _, A.At r2 _) = (head others, last others) in
      Result.throw (Error.TupleLargerThanThree tupleRegion (R.merge r1 r2))


processList :: Env.Env -> Can.Destructor -> [Src.Pattern] -> Result DupsDict w [Can.Pattern]
processList env destructor list =
  case list of
    [] ->
      Result.ok []

    pattern : otherPatterns ->
      (:)
        <$> process env (Can.DIndex Index.first destructor) pattern
        <*> processList env (Can.DIndex Index.second destructor) otherPatterns



-- LOG BINDINGS


logVar :: N.Name -> R.Region -> Can.Destructor -> a -> Result DupsDict w a
logVar name region destructor value =
  Result.Result $ \bindings warnings _ ok ->
    ok
      (Dups.insert name region () (A.At region destructor) bindings)
      warnings
      value


logFields :: [A.Located N.Name] -> Can.Destructor -> a -> Result DupsDict w a
logFields fields destructor value =
  let
    addField (A.At region name) dict =
      Dups.insert name region () (A.At region (Can.DField name destructor)) dict
  in
  Result.Result $ \bindings warnings _ ok ->
    ok (foldr addField bindings fields) warnings value
