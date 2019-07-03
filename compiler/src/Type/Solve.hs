{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Solve
  ( run
  )
  where


import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Type.Occurs as Occurs
import Type.Type as Type
import qualified Type.Error as ET
import qualified Type.Unify as Unify
import qualified Type.UnionFind as UF



-- RUN SOLVER


run :: Constraint -> IO (Either (NE.List Error.Error) (Map.Map Name.Name Can.Annotation))
run constraint =
  do  pools <- MVector.replicate 8 []

      (State env _ errors) <-
        solve Map.empty outermostRank pools emptyState constraint

      case errors of
        [] ->
          Right <$> traverse Type.toAnnotation env

        e:es ->
          return $ Left (NE.List e es)



{-# NOINLINE emptyState #-}
emptyState :: State
emptyState =
  State Map.empty (nextMark noMark) []



-- SOLVER


type Env =
  Map.Map Name.Name Variable


type Pools =
  MVector.IOVector [Variable]


data State =
  State
    { _env :: Env
    , _mark :: Mark
    , _errors :: [Error.Error]
    }


solve :: Env -> Int -> Pools -> State -> Constraint -> IO State
solve env rank pools state constraint =
  case constraint of
    CTrue ->
      return state

    CSaveTheEnvironment ->
      return (state { _env = env })

    CEqual region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region category actualType $
                      Error.typeReplace expectation expectedType

    CLocal region name expectation ->
      do  actual <- makeCopy rank pools (env ! name)
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Local name) actualType $
                      Error.typeReplace expectation expectedType

    CForeign region name (Can.Forall freeVars srcType) expectation ->
      do  actual <- srcTypeToVariable rank pools freeVars srcType
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Foreign name) actualType $
                      Error.typeReplace expectation expectedType

    CPattern region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- patternExpectationToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadPattern region category actualType
                      (Error.ptypeReplace expectation expectedType)

    CAnd constraints ->
      foldM (solve env rank pools) state constraints

    CLet [] flexs _ headerCon CTrue ->
      do  introduce rank pools flexs
          solve env rank pools state headerCon

    CLet [] [] header headerCon subCon ->
      do  state1 <- solve env rank pools state headerCon
          locals <- traverse (A.traverse (typeToVariable rank pools)) header
          let newEnv = Map.union env (Map.map A.toValue locals)
          state2 <- solve newEnv rank pools state1 subCon
          foldM occurs state2 $ Map.toList locals

    CLet rigids flexs header headerCon subCon ->
      do
          -- work in the next pool to localize header
          let nextRank = rank + 1
          let poolsLength = MVector.length pools
          nextPools <-
            if nextRank < poolsLength
              then return pools
              else MVector.grow pools poolsLength

          -- introduce variables
          let vars = rigids ++ flexs
          forM_ vars $ \var ->
            UF.modify var $ \(Descriptor content _ mark copy) ->
              Descriptor content nextRank mark copy
          MVector.write nextPools nextRank vars

          -- run solver in next pool
          locals <- traverse (A.traverse (typeToVariable nextRank nextPools)) header
          (State savedEnv mark errors) <-
            solve env nextRank nextPools state headerCon

          let youngMark = mark
          let visitMark = nextMark youngMark
          let finalMark = nextMark visitMark

          -- pop pool
          generalize youngMark visitMark nextRank nextPools
          MVector.write nextPools nextRank []

          -- check that things went well
          mapM_ isGeneric rigids

          let newEnv = Map.union env (Map.map A.toValue locals)
          let tempState = State savedEnv finalMark errors
          newState <- solve newEnv rank nextPools tempState subCon

          foldM occurs newState (Map.toList locals)


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> IO ()
isGeneric var =
  do  (Descriptor _ rank _ _) <- UF.get var
      if rank == noRank
        then return ()
        else
          do  tipe <- Type.toErrorType var
              error $
                "You ran into a compiler bug. Here are some details for the developers:\n\n"
                ++ "    " ++ show (ET.toDoc L.empty RT.None tipe) ++ " [rank = " ++ show rank ++ "]\n\n"
                ++
                  "Please create an <http://sscce.org/> and then report it\n\
                  \at <https://github.com/elm/compiler/issues>\n\n"



-- EXPECTATIONS TO VARIABLE


expectedToVariable :: Int -> Pools -> Error.Expected Type -> IO Variable
expectedToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.NoExpectation tipe ->
        tipe

      Error.FromContext _ _ tipe ->
        tipe

      Error.FromAnnotation _ _ _ tipe ->
        tipe


patternExpectationToVariable :: Int -> Pools -> Error.PExpected Type -> IO Variable
patternExpectationToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.PNoExpectation tipe ->
        tipe

      Error.PFromContext _ _ tipe ->
        tipe



-- ERROR HELPERS


addError :: State -> Error.Error -> State
addError (State savedEnv rank errors) err =
  State savedEnv rank (err:errors)



-- OCCURS CHECK


occurs :: State -> (Name.Name, A.Located Variable) -> IO State
occurs state (name, A.At region variable) =
  do  hasOccurred <- Occurs.occurs variable
      if hasOccurred
        then
          do  errorType <- Type.toErrorType variable
              (Descriptor _ rank mark copy) <- UF.get variable
              UF.set variable (Descriptor Error rank mark copy)
              return $ addError state (Error.InfiniteType region name errorType)
        else
          return state



-- GENERALIZE


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: Mark -> Mark -> Int -> Pools -> IO ()
generalize youngMark visitMark youngRank pools =
  do  youngVars <- MVector.read pools youngRank
      rankTable <- poolToRankTable youngMark youngRank youngVars

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      Vector.imapM_
        (\rank table -> mapM_ (adjustRank youngMark visitMark rank) table)
        rankTable

      -- For variables that have rank lowerer than youngRank, register them in
      -- the appropriate old pool if they are not redundant.
      Vector.forM_ (Vector.unsafeInit rankTable) $ \vars ->
        forM_ vars $ \var ->
          do  isRedundant <- UF.redundant var
              if isRedundant
                then return ()
                else
                  do  (Descriptor _ rank _ _) <- UF.get var
                      MVector.modify pools (var:) rank

      -- For variables with rank youngRank
      --   If rank < youngRank: register in oldPool
      --   otherwise generalize
      forM_ (Vector.unsafeLast rankTable) $ \var ->
        do  isRedundant <- UF.redundant var
            if isRedundant
              then return ()
              else
                do  (Descriptor content rank mark copy) <- UF.get var
                    if rank < youngRank
                      then MVector.modify pools (var:) rank
                      else UF.set var $ Descriptor content noRank mark copy


poolToRankTable :: Mark -> Int -> [Variable] -> IO (Vector.Vector [Variable])
poolToRankTable youngMark youngRank youngInhabitants =
  do  mutableTable <- MVector.replicate (youngRank + 1) []

      -- Sort the youngPool variables into buckets by rank.
      forM_ youngInhabitants $ \var ->
        do  (Descriptor content rank _ copy) <- UF.get var
            UF.set var (Descriptor content rank youngMark copy)
            MVector.modify mutableTable (var:) rank

      Vector.unsafeFreeze mutableTable



-- ADJUST RANK

--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--
adjustRank :: Mark -> Mark -> Int -> Variable -> IO Int
adjustRank youngMark visitMark groupRank var =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == youngMark then
          do  -- Set the variable as marked first because it may be cyclic.
              UF.set var $ Descriptor content rank visitMark copy
              maxRank <- adjustRankContent youngMark visitMark groupRank content
              UF.set var $ Descriptor content maxRank visitMark copy
              return maxRank

        else if mark == visitMark then
          return rank

        else
          do  let minRank = min groupRank rank
              -- TODO how can minRank ever be groupRank?
              UF.set var $ Descriptor content minRank visitMark copy
              return minRank


adjustRankContent :: Mark -> Mark -> Int -> Content -> IO Int
adjustRankContent youngMark visitMark groupRank content =
  let
    go = adjustRank youngMark visitMark groupRank
  in
    case content of
      FlexVar _ ->
          return groupRank

      FlexSuper _ _ ->
          return groupRank

      RigidVar _ ->
          return groupRank

      RigidSuper _ _ ->
          return groupRank

      Structure flatType ->
        case flatType of
          App1 _ _ args ->
            foldM (\rank arg -> max rank <$> go arg) outermostRank args

          Fun1 arg result ->
              max <$> go arg <*> go result

          EmptyRecord1 ->
              -- THEORY: an empty record never needs to get generalized
              return outermostRank

          Record1 fields extension ->
              do  extRank <- go extension
                  foldM (\rank field -> max rank <$> go field) extRank fields

          Unit1 ->
              -- THEORY: a unit never needs to get generalized
              return outermostRank

          Tuple1 a b maybeC ->
              do  ma <- go a
                  mb <- go b
                  case maybeC of
                    Nothing ->
                      return (max ma mb)

                    Just c ->
                      max (max ma mb) <$> go c

      Alias _ _ args _ ->
          -- THEORY: anything in the realVar would be outermostRank
          foldM (\rank (_, argVar) -> max rank <$> go argVar) outermostRank args

      Error ->
          return groupRank



-- REGISTER VARIABLES


introduce :: Int -> Pools -> [Variable] -> IO ()
introduce rank pools variables =
  do  MVector.modify pools (variables++) rank
      forM_ variables $ \var ->
        UF.modify var $ \(Descriptor content _ mark copy) ->
          Descriptor content rank mark copy



-- TYPE TO VARIABLE


typeToVariable :: Int -> Pools -> Type -> IO Variable
typeToVariable rank pools tipe =
  typeToVar rank pools Map.empty tipe


-- PERF working with @mgriffith we noticed that a 784 line entry in a `let` was
-- causing a ~1.5 second slowdown. Moving it to the top-level to be a function
-- saved all that time. The slowdown seems to manifest in `typeToVar` and in
-- `register` in particular. Have not explored further yet. Top-level definitions
-- are recommended in cases like this anyway, so there is at least a safety
-- valve for now.
--
typeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Type -> IO Variable
typeToVar rank pools aliasDict tipe =
  let go = typeToVar rank pools aliasDict in
  case tipe of
    VarN v ->
      return v

    AppN home name args ->
      do  argVars <- traverse go args
          register rank pools (Structure (App1 home name argVars))

    FunN a b ->
      do  aVar <- go a
          bVar <- go b
          register rank pools (Structure (Fun1 aVar bVar))

    AliasN home name args aliasType ->
      do  argVars <- traverse (traverse go) args
          aliasVar <- typeToVar rank pools (Map.fromList argVars) aliasType
          register rank pools (Alias home name argVars aliasVar)

    PlaceHolder name ->
      return (aliasDict ! name)

    RecordN fields ext ->
      do  fieldVars <- traverse go fields
          extVar <- go ext
          register rank pools (Structure (Record1 fieldVars extVar))

    EmptyRecordN ->
      register rank pools emptyRecord1

    UnitN ->
      register rank pools unit1

    TupleN a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- traverse go c
          register rank pools (Structure (Tuple1 aVar bVar cVar))


register :: Int -> Pools -> Content -> IO Variable
register rank pools content =
  do  var <- UF.fresh (Descriptor content rank noMark Nothing)
      MVector.modify pools (var:) rank
      return var


{-# NOINLINE emptyRecord1 #-}
emptyRecord1 :: Content
emptyRecord1 =
  Structure EmptyRecord1


{-# NOINLINE unit1 #-}
unit1 :: Content
unit1 =
  Structure Unit1



-- SOURCE TYPE TO VARIABLE


srcTypeToVariable :: Int -> Pools -> Map.Map Name.Name () -> Can.Type -> IO Variable
srcTypeToVariable rank pools freeVars srcType =
  let
    nameToContent name
      | Name.isNumberType     name = FlexSuper Number (Just name)
      | Name.isComparableType name = FlexSuper Comparable (Just name)
      | Name.isAppendableType name = FlexSuper Appendable (Just name)
      | Name.isCompappendType name = FlexSuper CompAppend (Just name)
      | otherwise                  = FlexVar (Just name)

    makeVar name _ =
      UF.fresh (Descriptor (nameToContent name) rank noMark Nothing)
  in
  do  flexVars <- Map.traverseWithKey makeVar freeVars
      MVector.modify pools (Map.elems flexVars ++) rank
      srcTypeToVar rank pools flexVars srcType


srcTypeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Can.Type -> IO Variable
srcTypeToVar rank pools flexVars srcType =
  let go = srcTypeToVar rank pools flexVars in
  case srcType of
    Can.TLambda argument result ->
      do  argVar <- go argument
          resultVar <- go result
          register rank pools (Structure (Fun1 argVar resultVar))

    Can.TVar name ->
      return (flexVars ! name)

    Can.TType home name args ->
      do  argVars <- traverse go args
          register rank pools (Structure (App1 home name argVars))

    Can.TRecord fields maybeExt ->
      do  fieldVars <- traverse (srcFieldTypeToVar rank pools flexVars) fields
          extVar <-
            case maybeExt of
              Nothing -> register rank pools emptyRecord1
              Just ext -> return (flexVars ! ext)
          register rank pools (Structure (Record1 fieldVars extVar))

    Can.TUnit ->
      register rank pools unit1

    Can.TTuple a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- traverse go c
          register rank pools (Structure (Tuple1 aVar bVar cVar))

    Can.TAlias home name args aliasType ->
      do  argVars <- traverse (traverse go) args
          aliasVar <-
            case aliasType of
              Can.Holey tipe ->
                srcTypeToVar rank pools (Map.fromList argVars) tipe

              Can.Filled tipe ->
                go tipe

          register rank pools (Alias home name argVars aliasVar)


srcFieldTypeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Can.FieldType -> IO Variable
srcFieldTypeToVar rank pools flexVars (Can.FieldType _ srcTipe) =
  srcTypeToVar rank pools flexVars srcTipe



-- COPY


makeCopy :: Int -> Pools -> Variable -> IO Variable
makeCopy rank pools var =
  do  copy <- makeCopyHelp rank pools var
      restore var
      return copy


makeCopyHelp :: Int -> Pools -> Variable -> IO Variable
makeCopyHelp maxRank pools variable =
  do  (Descriptor content rank _ maybeCopy) <- UF.get variable

      case maybeCopy of
        Just copy ->
          return copy

        Nothing ->
          if rank /= noRank then
            return variable

          else
            do  let makeDescriptor c = Descriptor c maxRank noMark Nothing
                copy <- UF.fresh $ makeDescriptor content
                MVector.modify pools (copy:) maxRank

                -- Link the original variable to the new variable. This lets us
                -- avoid making multiple copies of the variable we are instantiating.
                --
                -- Need to do this before recursively copying to avoid looping.
                UF.set variable $
                  Descriptor content rank noMark (Just copy)

                -- Now we recursively copy the content of the variable.
                -- We have already marked the variable as copied, so we
                -- will not repeat this work or crawl this variable again.
                case content of
                  Structure term ->
                    do  newTerm <- traverseFlatType (makeCopyHelp maxRank pools) term
                        UF.set copy $ makeDescriptor (Structure newTerm)
                        return copy

                  FlexVar _ ->
                    return copy

                  FlexSuper _ _ ->
                    return copy

                  RigidVar name ->
                    do  UF.set copy $ makeDescriptor $ FlexVar (Just name)
                        return copy

                  RigidSuper super name ->
                    do  UF.set copy $ makeDescriptor $ FlexSuper super (Just name)
                        return copy

                  Alias home name args realType ->
                    do  newArgs <- mapM (traverse (makeCopyHelp maxRank pools)) args
                        newRealType <- makeCopyHelp maxRank pools realType
                        UF.set copy $ makeDescriptor (Alias home name newArgs newRealType)
                        return copy

                  Error ->
                    return copy



-- RESTORE


restore :: Variable -> IO ()
restore variable =
  do  (Descriptor content _ _ maybeCopy) <- UF.get variable
      case maybeCopy of
        Nothing ->
          return ()

        Just _ ->
          do  UF.set variable $ Descriptor content noRank noMark Nothing
              restoreContent content


restoreContent :: Content -> IO ()
restoreContent content =
  case content of
    FlexVar _ ->
      return ()

    FlexSuper _ _ ->
      return ()

    RigidVar _ ->
      return ()

    RigidSuper _ _ ->
      return ()

    Structure term ->
      case term of
        App1 _ _ args ->
          mapM_ restore args

        Fun1 arg result ->
          do  restore arg
              restore result

        EmptyRecord1 ->
          return ()

        Record1 fields ext ->
          do  mapM_ restore fields
              restore ext

        Unit1 ->
          return ()

        Tuple1 a b maybeC ->
          do  restore a
              restore b
              case maybeC of
                Nothing -> return ()
                Just c  -> restore c

    Alias _ _ args var ->
      do  mapM_ (traverse restore) args
          restore var

    Error ->
        return ()



-- TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> IO Variable) -> FlatType -> IO FlatType
traverseFlatType f flatType =
  case flatType of
    App1 home name args ->
        liftM (App1 home name) (traverse f args)

    Fun1 a b ->
        liftM2 Fun1 (f a) (f b)

    EmptyRecord1 ->
        pure EmptyRecord1

    Record1 fields ext ->
        liftM2 Record1 (traverse f fields) (f ext)

    Unit1 ->
        pure Unit1

    Tuple1 a b cs ->
        liftM3 Tuple1 (f a) (f b) (traverse f cs)
