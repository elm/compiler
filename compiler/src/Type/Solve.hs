{-# LANGUAGE TemplateHaskell #-}
module Type.Solve
  ( run
  )
  where


import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyList as NE
import qualified Data.NonEmptyMap as NEM

import qualified Array as A
import qualified Crash

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeIndex as T
import qualified Elm.Interface as I
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Render.Type as RT
import qualified Type.Occurs as Occurs
import qualified Type.Pools as Pools
import Type.Type as Type
import qualified Type.Unify as Unify
import qualified Type.UnionFind as UF



-- RUN SOLVER


run :: Constraint -> IO (Either (NE.List Error.Error) (Map.Map N.Name Variable))
run constraint =
  do  pools <- Pools.init 8

      (State env _ errors) <-
        solve Map.empty outermostRank pools emptyState constraint

      case errors of
        []   -> return $ Right env
        x:xs -> return $ Left (NE.List x xs)


{-# NOINLINE emptyState #-}
emptyState :: State
emptyState =
  State Map.empty (incrMark noMark) []



-- SOLVER


type Env =
  Map.Map N.Name Variable


data State =
  State
    { _env :: Env
    , _mark :: Mark
    , _errors :: [Error.Error]
    }


solve :: Env -> Rank -> Pools.Pools -> State -> Constraint -> IO State
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
      do  actual <- makeCopy rank pools $ $(Map.require 'solve) name env N.toChars
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

    CForeign region name annotation expectation ->
      do  actual <- annotationToVariable rank pools annotation
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

    COperator region op annotation expectation ->
      do  actual <- annotationToVariable rank pools annotation
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Operator op) actualType $
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
          let nextRank = incrRank rank
          nextPools <- Pools.growIfNeeded nextRank pools

          -- introduce variables
          let vars = rigids ++ flexs
          forM_ vars $ \var ->
            UF.modify var $ \(Descriptor content _ mark copy) ->
              Descriptor content nextRank mark copy
          Pools.write nextRank vars nextPools

          -- run solver in next pool
          locals <- traverse (A.traverse (typeToVariable nextRank nextPools)) header
          (State savedEnv mark errors) <-
            solve env nextRank nextPools state headerCon

          let youngMark = mark
          let visitMark = incrMark youngMark
          let finalMark = incrMark visitMark

          -- pop pool
          generalize youngMark visitMark nextRank nextPools
          Pools.write nextRank [] nextPools

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
              let (Rank r) = rank
              $(Crash.crashIO 'isGeneric) $
                "You ran into a compiler bug. Here are some details for the developers:\n\n"
                ++ "    " ++ show (RT.errToDoc RT.emptyLocalizer RT.None tipe) ++ " [rank = " ++ show r ++ "]\n\n"
                ++
                  "Please create an <http://sscce.org/> and then report it\n\
                  \at <https://github.com/elm/compiler/issues>\n\n"



-- EXPECTATIONS TO VARIABLE


expectedToVariable :: Rank -> Pools.Pools -> Error.Expected Type -> IO Variable
expectedToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.NoExpectation        t -> t
      Error.FromContext      _ _ t -> t
      Error.FromAnnotation _ _ _ t -> t


patternExpectationToVariable :: Rank -> Pools.Pools -> Error.PExpected Type -> IO Variable
patternExpectationToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.PNoExpectation   t -> t
      Error.PFromContext _ _ t -> t



-- ERROR HELPERS


addError :: State -> Error.Error -> State
addError (State savedEnv rank errors) err =
  State savedEnv rank (err:errors)



-- OCCURS CHECK


occurs :: State -> (N.Name, A.Located Variable) -> IO State
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
--
-- Every variable has rank less than or equal to the maxRank of the pool.
-- This sorts variables into the young and old pools accordingly.


generalize :: Mark -> Mark -> Rank -> Pools.Pools -> IO ()
generalize youngMark visitMark youngRank pools =
  do  youngVars <- Pools.read youngRank pools

      -- Sort the youngPool variables into buckets by rank.
      rankTable <- Pools.rankTable youngRank youngVars $ \var ->
        do  (Descriptor content rank _ copy) <- UF.get var
            UF.set var (Descriptor content rank youngMark copy)
            return rank

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      Pools.indexedTraverse_
        (\rank table -> mapM_ (adjustRank youngMark visitMark rank) table)
        rankTable

      Pools.traverseRanks_
        (
          -- For variables that have rank lowerer than youngRank, register them in
          -- the appropriate old pool if they are not redundant.
          \var ->
              do  isRedundant <- UF.redundant var
                  if isRedundant
                    then return ()
                    else
                      do  (Descriptor _ rank _ _) <- UF.get var
                          Pools.modify rank (var:) pools
        )
        (
          -- For variables with rank youngRank
          --   If rank < youngRank: register in oldPool
          --   otherwise generalize
          \var ->
              do  isRedundant <- UF.redundant var
                  if isRedundant
                    then return ()
                    else
                      do  (Descriptor content rank mark copy) <- UF.get var
                          if rank < youngRank
                            then Pools.modify rank (var:) pools
                            else UF.set var $ Descriptor content noRank mark copy
        )
        rankTable



-- ADJUST RANK
--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.


adjustRank :: Mark -> Mark -> Rank -> Variable -> IO Rank
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


adjustRankContent :: Mark -> Mark -> Rank -> Content -> IO Rank
adjustRankContent youngMark visitMark groupRank content =
  case content of
    Flex             _ -> return groupRank
    Rigid            _ -> return groupRank
    Error              -> return groupRank
    Alias _ _ _ xs _ _ -> A.foldlM step outermostRank xs -- THEORY: anything in the realVar would be outermostRank
    Structure flatType ->
      case flatType of
        App1 _ _ _ xs  -> A.foldlM step outermostRank xs
        Fun1 x y       -> max <$> go x <*> go y
        Unit1          -> return outermostRank -- THEORY: a unit never needs to get generalized
        Pair1   a b    -> step a =<< go b
        Triple1 a b c  -> step a =<< step b =<< go c
        Empty1         -> return outermostRank -- THEORY: an empty record never needs to get generalized
        Record1 _ fs   -> NEM.foldrM step outermostRank fs
        ExtRec1 _ fs e -> step e =<< NEM.foldrM step outermostRank fs
  where
    go = adjustRank youngMark visitMark groupRank
    step var rank = max rank <$> go var



-- REGISTER VARIABLES


introduce :: Rank -> Pools.Pools -> [Variable] -> IO ()
introduce rank pools variables =
  do  Pools.modify rank (variables++) pools
      forM_ variables $ \var ->
        UF.modify var $ \(Descriptor content _ mark copy) ->
          Descriptor content rank mark copy



-- TYPE TO VARIABLE


typeToVariable :: Rank -> Pools.Pools -> Type -> IO Variable
typeToVariable rank pools tipe =
  case tipe of
    VarN v ->
      return v

    AppN h n mask args ->
      do  argVars <- A.traverseIO (typeToVariable rank pools) args
          register rank pools (Structure (App1 h n mask argVars))

    FunN a b ->
      do  aVar <- typeToVariable rank pools a
          bVar <- typeToVariable rank pools b
          register rank pools (Structure (Fun1 aVar bVar))

    TypeN vs t ->
      annotationToVar rank pools vs t

    UnitN ->
      register rank pools (Structure Unit1)

    PairN a b ->
      do  aVar <- typeToVariable rank pools a
          bVar <- typeToVariable rank pools b
          register rank pools (Structure (Pair1 aVar bVar))

    TripleN a b c ->
      do  aVar <- typeToVariable rank pools a
          bVar <- typeToVariable rank pools b
          cVar <- typeToVariable rank pools c
          register rank pools (Structure (Triple1 aVar bVar cVar))

    EmptyN ->
      register rank pools (Structure Empty1)

    RecordN order fields ->
      do  fieldVars <- NEM.traverse (typeToVariable rank pools) fields
          register rank pools (Structure (Record1 order fieldVars))

    ExtRecN order fields ext ->
      do  fieldVars <- NEM.traverse (typeToVariable rank pools) fields
          extVar <- typeToVariable rank pools ext
          register rank pools (Structure (ExtRec1 order fieldVars extVar))


register :: Rank -> Pools.Pools -> Content -> IO Variable
register rank pools content =
  do  var <- UF.fresh (Descriptor content rank noMark Nothing)
      Pools.modify rank (var:) pools
      return var



-- ANNOTATION TO VARIABLE


annotationToVariable :: Rank -> Pools.Pools -> I.Annotation -> IO Variable
annotationToVariable rank pools (I.Forall freeVars tipe) =
  do  flexVars <- A.traverseIO makeVar freeVars
      Pools.modify rank (A.addToList flexVars) pools
      annotationToVar rank pools flexVars tipe
  where
    makeVar var =
      UF.fresh $ Descriptor (Flex var) rank noMark Nothing



-- PERF working with @mgriffith we noticed that a 784 line entry in a `let` was
-- causing a ~1.5 second slowdown. Moving it to the top-level to be a function
-- saved all that time. The slowdown seems to manifest in `typeToVar` and in
-- `register` in particular. Have not explored further yet. Top-level definitions
-- are recommended in cases like this anyway, so there is at least a safety
-- valve for now.
--


annotationToVar :: Rank -> Pools.Pools -> A.Array Variable -> I.Type -> IO Variable
annotationToVar rank pools flexVars =
    go
  where
    go tipe =
      case tipe of
        I.TLambda x y ys ->
          do  z  <- go x
              zs <- goLambda y ys
              register rank pools (Structure (Fun1 z zs))

        I.TVar index ->
          return (T.get index flexVars)

        I.TType h n m args ->
          do  argVars <- A.traverseIO go args
              register rank pools (Structure (App1 h n m argVars))

        I.TAlias h n m args aliased@(I.Aliased t) ->
          do  argVars  <- A.traverseIO go args
              aliasVar <- annotationToVar rank pools argVars t
              register rank pools (Alias h n m argVars aliased aliasVar)

        I.TUnit ->
          register rank pools (Structure Unit1)

        I.TPair a b ->
          do  aVar <- go a
              bVar <- go b
              register rank pools (Structure (Pair1 aVar bVar))

        I.TTriple a b c ->
          do  aVar <- go a
              bVar <- go b
              cVar <- go c
              register rank pools (Structure (Triple1 aVar bVar cVar))

        I.TEmpty ->
          register rank pools (Structure Empty1)

        I.TRecord (F.Fields order dict) ->
          do  fieldVars <- NEM.traverse go dict
              register rank pools (Structure (Record1 order fieldVars))

        I.TExtRec (F.Fields order dict) ext ->
          do  fieldVars <- NEM.traverse go dict
              let extVar = T.get ext flexVars
              register rank pools (Structure (ExtRec1 order fieldVars extVar))

    goLambda x xs =
      case xs of
        []   -> go x
        y:ys ->
          do  z  <- go x
              zs <- goLambda y ys
              register rank pools (Structure (Fun1 z zs))



-- COPY


makeCopy :: Rank -> Pools.Pools -> Variable -> IO Variable
makeCopy rank pools var =
  do  copy <- makeCopyHelp rank pools var
      restore var
      return copy


makeCopyHelp :: Rank -> Pools.Pools -> Variable -> IO Variable
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
                Pools.modify maxRank (copy:) pools

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

                  Flex _ ->
                    return copy

                  Rigid var ->
                    do  UF.set copy $ makeDescriptor $ Flex var
                        return copy

                  Alias h n m args aliasedType realType ->
                    do  newArgs <- A.traverseIO (makeCopyHelp maxRank pools) args
                        newRealType <- makeCopyHelp maxRank pools realType
                        UF.set copy $ makeDescriptor (Alias h n m newArgs aliasedType newRealType)
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
    Flex             _ -> return ()
    Rigid            _ -> return ()
    Error              -> return ()
    Alias _ _ _ xs _ v -> A.traverse_ restore xs >> restore v
    Structure term ->
      case term of
        App1 _ _ _ xs  -> A.traverse_ restore xs
        Fun1 x y       -> restore x >> restore y
        Unit1          -> return ()
        Pair1   a b    -> restore a >> restore b
        Triple1 a b c  -> restore a >> restore b >> restore c
        Empty1         -> return ()
        Record1 _ fs   -> NEM.traverse_ restore fs
        ExtRec1 _ fs e -> NEM.traverse_ restore fs >> restore e



-- TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> IO Variable) -> FlatType -> IO FlatType
traverseFlatType go flatType =
  case flatType of
    App1 h n m xs  -> liftM (App1 h n m) (A.traverseIO go xs)
    Fun1 x y       -> liftM2 Fun1 (go x) (go y)
    Unit1          -> return Unit1
    Pair1   a b    -> liftM2 Pair1   (go a) (go b)
    Triple1 a b c  -> liftM3 Triple1 (go a) (go b) (go c)
    Empty1         -> return Empty1
    Record1 o fs   -> liftM  (Record1 o) (NEM.traverse go fs)
    ExtRec1 o fs e -> liftM2 (ExtRec1 o) (NEM.traverse go fs) (go e)

