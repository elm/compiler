{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Expression
  ( canonicalize
  , FreeLocals
  , Uses(..)
  , verifyBindings
  , gatherTypedArgs
  )
  where


import Control.Monad (foldM)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Internal as I

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Type as Type
import qualified AST.Valid as Valid
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- RESULTS


type Result i w a =
  Result.Result i w Error.Error a


type FreeLocals =
  Map.Map N.Name Uses


data Uses =
  Uses
    { _direct :: {-# UNPACK #-} !Int
    , _delayed :: {-# UNPACK #-} !Int
    }



-- CANONICALIZE


canonicalize :: Env.Env -> Valid.Expr -> Result FreeLocals [W.Warning] Can.Expr
canonicalize env (A.At region expression) =
  A.At region <$>
  case expression of
    Valid.Str string ->
      Result.ok (Can.Str string)

    Valid.Chr char ->
      Result.ok (Can.Chr char)

    Valid.Int int ->
      Result.ok (Can.Int int)

    Valid.Float float ->
      Result.ok (Can.Float float)

    Valid.Var varType name ->
      case varType of
        Src.Value -> findVar region env name
        Src.Ctor -> toVarCtor name <$> Env.findCtor region env name

    Valid.VarQual varType prefix name ->
      case varType of
        Src.Value -> findVarQual region env prefix name
        Src.Ctor -> toVarCtor name <$> Env.findCtorQual region env prefix name

    Valid.List exprs ->
      Can.List <$> traverse (canonicalize env) exprs

    Valid.Op op ->
      do  (Env.Binop _ home name annotation _ _) <- Env.findBinop region env op
          return (Can.VarOperator op home name annotation)

    Valid.Negate expr ->
      Can.Negate <$> canonicalize env expr

    Valid.Binops ops final ->
      A.toValue <$> canonicalizeBinops region env ops final

    Valid.Lambda srcArgs body ->
      delayedUsage $
      do  (args, bindings) <-
            Pattern.verify Error.DPLambdaArgs $
              traverse (Pattern.canonicalize env) srcArgs

          newEnv <-
            Env.addLocals bindings env

          (cbody, freeLocals) <-
            verifyBindings W.Pattern bindings (canonicalize newEnv body)

          return (Can.Lambda args cbody, freeLocals)

    Valid.Call func args ->
      Can.Call
        <$> canonicalize env func
        <*> traverse (canonicalize env) args

    Valid.If branches finally ->
      Can.If
        <$> traverse (canonicalizeIfBranch env) branches
        <*> canonicalize env finally

    Valid.Let defs expr ->
      A.toValue <$> canonicalizeLet region env defs expr

    Valid.Case expr branches ->
      Can.Case
        <$> canonicalize env expr
        <*> traverse (canonicalizeCaseBranch env) branches

    Valid.Accessor field ->
      Result.ok $ Can.Accessor field

    Valid.Access record field ->
      Can.Access
        <$> canonicalize env record
        <*> Result.ok field

    Valid.Update (A.At reg name) fields ->
      let
        makeCanFields =
          Dups.checkFields' (\r t -> Can.FieldUpdate r <$> canonicalize env t) fields
      in
      Can.Update name
        <$> (A.At reg <$> findVar reg env name)
        <*> (sequenceA =<< makeCanFields)

    Valid.Record fields ->
      do  fieldDict <- Dups.checkFields fields
          Can.Record <$> traverse (canonicalize env) fieldDict

    Valid.Unit ->
      Result.ok Can.Unit

    Valid.Tuple a b cs ->
      Can.Tuple
        <$> canonicalize env a
        <*> canonicalize env b
        <*> canonicalizeTupleExtras region env cs

    Valid.Shader uid src tipe ->
        Result.ok (Can.Shader uid src tipe)



-- CANONICALIZE TUPLE EXTRAS


canonicalizeTupleExtras :: R.Region -> Env.Env -> [Valid.Expr] -> Result FreeLocals [W.Warning] (Maybe Can.Expr)
canonicalizeTupleExtras region env extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> canonicalize env three

    _ ->
      Result.throw (Error.TupleLargerThanThree region)



-- CANONICALIZE IF BRANCH


canonicalizeIfBranch :: Env.Env -> (Valid.Expr, Valid.Expr) -> Result FreeLocals [W.Warning] (Can.Expr, Can.Expr)
canonicalizeIfBranch env (condition, branch) =
  (,)
    <$> canonicalize env condition
    <*> canonicalize env branch



-- CANONICALIZE CASE BRANCH


canonicalizeCaseBranch :: Env.Env -> (Src.Pattern, Valid.Expr) -> Result FreeLocals [W.Warning] Can.CaseBranch
canonicalizeCaseBranch env (pattern, expr) =
  directUsage $
  do  (cpattern, bindings) <-
        Pattern.verify Error.DPCaseBranch $
          Pattern.canonicalize env pattern
      newEnv <- Env.addLocals bindings env

      (cexpr, freeLocals) <-
        verifyBindings W.Pattern bindings (canonicalize newEnv expr)

      return (Can.CaseBranch cpattern cexpr, freeLocals)



-- CANONICALIZE BINOPS


canonicalizeBinops :: R.Region -> Env.Env -> [(Valid.Expr, A.Located N.Name)] -> Valid.Expr -> Result FreeLocals [W.Warning] Can.Expr
canonicalizeBinops overallRegion env ops final =
  let
    canonicalizeHelp (expr, A.At region op) =
      (,)
        <$> canonicalize env expr
        <*> Env.findBinop region env op
  in
  runBinopStepper overallRegion =<< (
    More
      <$> traverse canonicalizeHelp ops
      <*> canonicalize env final
  )


data Step
  = Done Can.Expr
  | More [(Can.Expr, Env.Binop)] Can.Expr
  | Error Env.Binop Env.Binop


runBinopStepper :: R.Region -> Step -> Result FreeLocals w Can.Expr
runBinopStepper overallRegion step =
  case step of
    Done expr ->
      Result.ok expr

    More [] expr ->
      Result.ok expr

    More ( (expr, op) : rest ) final ->
      runBinopStepper overallRegion $
        toBinopStep (toBinop op expr) op rest final

    Error (Env.Binop op1 _ _ _ _ _) (Env.Binop op2 _ _ _ _ _) ->
      Result.throw (Error.Binop overallRegion op1 op2)


toBinopStep :: (Can.Expr -> Can.Expr) -> Env.Binop -> [(Can.Expr, Env.Binop)] -> Can.Expr -> Step
toBinopStep makeBinop rootOp@(Env.Binop _ _ _ _ rootAssociativity rootPrecedence) middle final =
  case middle of
    [] ->
      Done (makeBinop final)

    ( expr, op@(Env.Binop _ _ _ _ associativity precedence) ) : rest ->
      if precedence < rootPrecedence then

        More ((makeBinop expr, op) : rest) final

      else if precedence > rootPrecedence then

        case toBinopStep (toBinop op expr) op rest final of
          Done newLast ->
            Done (makeBinop newLast)

          More newMiddle newLast ->
            toBinopStep makeBinop rootOp newMiddle newLast

          Error a b ->
            Error a b

      else

        case (rootAssociativity, associativity) of
          (Binop.Left, Binop.Left) ->
            toBinopStep (\right -> toBinop op (makeBinop expr) right) op rest final

          (Binop.Right, Binop.Right) ->
            toBinopStep (\right -> makeBinop (toBinop op expr right)) op rest final

          (_, _) ->
            Error rootOp op


toBinop :: Env.Binop -> Can.Expr -> Can.Expr -> Can.Expr
toBinop (Env.Binop op home name annotation _ _) left right =
  A.merge left right (Can.Binop op home name annotation left right)



-- CANONICALIZE LET


canonicalizeLet :: R.Region -> Env.Env -> [Valid.Def] -> Valid.Expr -> Result FreeLocals [W.Warning] Can.Expr
canonicalizeLet letRegion env defs body =
  directUsage $
    do  bindings <-
          Dups.detect (Error.DuplicatePattern Error.DPLetBinding) $
            List.foldl' addBindings Dups.none defs

        newEnv <- Env.addLocals bindings env

        verifyBindings W.Def bindings $
          do  nodes <- foldM (addDefNodes newEnv) [] defs
              cbody <- canonicalize newEnv body
              detectCycles letRegion (Graph.stronglyConnComp nodes) cbody



-- ADD BINDINGS


addBindings :: Dups.Dict R.Region -> Valid.Def -> Dups.Dict R.Region
addBindings bindings def =
  case def of
    Valid.Define _ (A.At region name) _ _ _ ->
      Dups.insert name region region bindings

    Valid.Destruct _ pattern _ ->
      addBindingsHelp bindings pattern


addBindingsHelp :: Dups.Dict R.Region -> Src.Pattern -> Dups.Dict R.Region
addBindingsHelp bindings (A.At region pattern) =
  case pattern of
    Src.PAnything ->
      bindings

    Src.PVar name ->
      Dups.insert name region region bindings

    Src.PRecord fields ->
      let
        addField dict (A.At fieldRegion name) =
          Dups.insert name fieldRegion fieldRegion dict
      in
      List.foldl' addField bindings fields

    Src.PUnit ->
      bindings

    Src.PTuple a b cs ->
      List.foldl' addBindingsHelp bindings (a:b:cs)

    Src.PCtor _ _ patterns ->
      List.foldl' addBindingsHelp bindings patterns

    Src.PCtorQual _ _ _ patterns ->
      List.foldl' addBindingsHelp bindings patterns

    Src.PList patterns ->
      List.foldl' addBindingsHelp bindings patterns

    Src.PCons hd tl ->
      addBindingsHelp (addBindingsHelp bindings hd) tl

    Src.PAlias aliasPattern (A.At nameRegion name) ->
      Dups.insert name nameRegion nameRegion $
        addBindingsHelp bindings aliasPattern

    Src.PChr _ ->
      bindings

    Src.PStr _ ->
      bindings

    Src.PInt _ ->
      bindings



-- BUILD BINDINGS GRAPH


type Node =
  (Binding, N.Name, [N.Name])


data Binding
  = Define Can.Def
  | Edge (A.Located N.Name)
  | Destruct Can.Pattern Can.Expr


addDefNodes :: Env.Env -> [Node] -> Valid.Def -> Result FreeLocals [W.Warning] [Node]
addDefNodes env nodes def =
  case def of
    Valid.Define _ aname@(A.At _ name) srcArgs body maybeType ->
      case maybeType of
        Nothing ->
          do  (args, argBindings) <-
                Pattern.verify (Error.DPFuncArgs name) $
                  traverse (Pattern.canonicalize env) srcArgs

              newEnv <-
                Env.addLocals argBindings env

              (cbody, freeLocals) <-
                verifyBindings W.Pattern argBindings (canonicalize newEnv body)

              let cdef = Can.Def aname args cbody
              let node = ( Define cdef, name, Map.keys freeLocals )
              logLetLocals args freeLocals (node:nodes)

        Just tipe ->
          do  (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
              ((args, resultType), argBindings) <-
                Pattern.verify (Error.DPFuncArgs name) $
                  gatherTypedArgs env name srcArgs ctipe Index.first []

              newEnv <-
                Env.addLocals argBindings env

              (cbody, freeLocals) <-
                verifyBindings W.Pattern argBindings (canonicalize newEnv body)

              let cdef = Can.TypedDef aname freeVars args cbody resultType
              let node = ( Define cdef, name, Map.keys freeLocals )
              logLetLocals args freeLocals (node:nodes)

    Valid.Destruct _ pattern body ->
      do  (cpattern, _bindings) <-
            Pattern.verify Error.DPDestruct $
              Pattern.canonicalize env pattern

          Result.Result $ \fs ws bad good ->
            case canonicalize env body of
              Result.Result k ->
                k Map.empty ws
                  (\freeLocals warnings errors ->
                      bad (Map.unionWith combineUses freeLocals fs) warnings errors
                  )
                  (\freeLocals warnings cbody ->
                      let
                        names = getPatternNames [] pattern
                        name = N.toCompositeName (map A.toValue names)
                        node = ( Destruct cpattern cbody, name, Map.keys freeLocals )
                      in
                      good
                        (Map.unionWith combineUses fs freeLocals)
                        warnings
                        (List.foldl' (addEdge [name]) (node:nodes) names)
                  )


logLetLocals :: [arg] -> FreeLocals -> value -> Result FreeLocals w value
logLetLocals args letLocals value =
  Result.Result $ \freeLocals warnings _ good ->
    good
      ( Map.unionWith combineUses freeLocals $
          case args of
            [] -> letLocals
            _ -> Map.map delayUse letLocals
      )
      warnings
      value


addEdge :: [N.Name] -> [Node] -> A.Located N.Name -> [Node]
addEdge edges nodes aname@(A.At _ name) =
  (Edge aname, name, edges) : nodes


getPatternNames :: [A.Located N.Name] -> Src.Pattern ->  [A.Located N.Name]
getPatternNames names (A.At region pattern) =
  case pattern of
    Src.PAnything            -> names
    Src.PVar name            -> A.At region name : names
    Src.PRecord fields       -> fields ++ names
    Src.PAlias ptrn name     -> getPatternNames (name : names) ptrn
    Src.PUnit                -> names
    Src.PTuple a b cs        -> List.foldl' getPatternNames (getPatternNames (getPatternNames names a) b) cs
    Src.PCtor _ _ args       -> List.foldl' getPatternNames names args
    Src.PCtorQual _ _ _ args -> List.foldl' getPatternNames names args
    Src.PList patterns       -> List.foldl' getPatternNames names patterns
    Src.PCons hd tl          -> getPatternNames (getPatternNames names hd) tl
    Src.PChr _               -> names
    Src.PStr _               -> names
    Src.PInt _               -> names



-- GATHER TYPED ARGS


gatherTypedArgs
  :: Env.Env
  -> N.Name
  -> [Src.Pattern]
  -> Can.Type
  -> Index.ZeroBased
  -> [(Can.Pattern, Can.Type)]
  -> Result Pattern.DupsDict w ([(Can.Pattern, Can.Type)], Can.Type)
gatherTypedArgs env name srcArgs tipe index revTypedArgs =
  case srcArgs of
    [] ->
      return (reverse revTypedArgs, tipe)

    srcArg : otherSrcArgs ->
      case Type.iteratedDealias tipe of
        Can.TLambda argType resultType ->
          do  arg <- Pattern.canonicalize env srcArg
              gatherTypedArgs env name otherSrcArgs resultType (Index.next index) $
                (arg, argType) : revTypedArgs

        _ ->
          let (A.At start _, A.At end _) = (head srcArgs, last srcArgs) in
          Result.throw $
            Error.AnnotationTooShort (R.merge start end) name index (length srcArgs)



-- DETECT CYCLES


detectCycles :: R.Region -> [Graph.SCC Binding] -> Can.Expr -> Result i w Can.Expr
detectCycles letRegion sccs body =
  case sccs of
    [] ->
      Result.ok body

    scc : subSccs ->
      case scc of
        Graph.AcyclicSCC binding ->
          case binding of
            Define def ->
              A.At letRegion . Can.Let def <$> detectCycles letRegion subSccs body

            Edge _ ->
              detectCycles letRegion subSccs body

            Destruct pattern expr ->
              A.At letRegion . Can.LetDestruct pattern expr <$> detectCycles letRegion subSccs body

        Graph.CyclicSCC bindings ->
          A.At letRegion <$>
            (Can.LetRec
              <$> checkCycle bindings []
              <*> detectCycles letRegion subSccs body
            )


checkCycle :: [Binding] -> [Can.Def] -> Result i w [Can.Def]
checkCycle bindings defs =
  case bindings of
    [] ->
      Result.ok defs

    binding : otherBindings ->
      case binding of
        Define def@(Can.Def name args _) ->
          if null args then
            Result.throw (Error.RecursiveLet name (toNames otherBindings defs))
          else
            checkCycle otherBindings (def:defs)

        Define def@(Can.TypedDef name _ args _ _) ->
          if null args then
            Result.throw (Error.RecursiveLet name (toNames otherBindings defs))
          else
            checkCycle otherBindings (def:defs)

        Edge name ->
          Result.throw (Error.RecursiveLet name (toNames otherBindings defs))

        Destruct _ _ ->
          -- a Destruct cannot appear in a cycle without any Edge values
          -- so we just keep going until we get to the edges
          checkCycle otherBindings defs


toNames :: [Binding] -> [Can.Def] -> [N.Name]
toNames bindings revDefs =
  case bindings of
    [] ->
      reverse (map getDefName revDefs)

    binding : otherBindings ->
      case binding of
        Define def         -> getDefName def : toNames otherBindings revDefs
        Edge (A.At _ name) -> name : toNames otherBindings revDefs
        Destruct _ _       -> toNames otherBindings revDefs


getDefName :: Can.Def -> N.Name
getDefName def =
  case def of
    Can.Def (A.At _ name) _ _ ->
      name

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      name



-- LOG VARIABLE USES


logVar :: N.Name -> a -> Result FreeLocals w a
logVar name value =
  Result.Result $ \freeLocals warnings _ good ->
    good (Map.insertWith combineUses name oneDirectUse freeLocals) warnings value


{-# NOINLINE oneDirectUse #-}
oneDirectUse :: Uses
oneDirectUse =
  Uses 1 0


combineUses :: Uses -> Uses -> Uses
combineUses (Uses a b) (Uses x y) =
  Uses (a + x) (b + y)


delayUse :: Uses -> Uses
delayUse (Uses direct delayed) =
  Uses 0 (direct + delayed)



-- MANAGING BINDINGS


verifyBindings
  :: W.Context
  -> Pattern.Bindings
  -> Result FreeLocals [W.Warning] value
  -> Result info [W.Warning] (value, FreeLocals)
verifyBindings context bindings (Result.Result k) =
  Result.Result $ \info warnings bad good ->
    k Map.empty warnings
      (\_ warnings1 err ->
          bad info warnings1 err
      )
      (\freeLocals warnings1 value ->
          let
            outerFreeLocals =
              Map.difference freeLocals bindings

            warnings2 =
              -- NOTE: Uses Map.size for O(1) lookup. This means there is
              -- no dictionary allocation unless a problem is detected.
              if Map.size bindings + Map.size outerFreeLocals == Map.size freeLocals then
                warnings1
              else
                Map.foldlWithKey (addUnusedWarning context) warnings1 $
                  Map.difference bindings freeLocals
          in
          good info warnings2 (value, outerFreeLocals)
      )


addUnusedWarning :: W.Context -> [W.Warning] -> N.Name -> R.Region -> [W.Warning]
addUnusedWarning context warnings name region =
  W.UnusedVariable region context name : warnings


directUsage :: Result () w (expr, FreeLocals) -> Result FreeLocals w expr
directUsage (Result.Result k) =
  Result.Result $ \freeLocals warnings bad good ->
    k () warnings
      (\() ws es -> bad freeLocals ws es)
      (\() ws (value, newFreeLocals) ->
          good (Map.unionWith combineUses freeLocals newFreeLocals) ws value
      )


delayedUsage :: Result () w (expr, FreeLocals) -> Result FreeLocals w expr
delayedUsage (Result.Result k) =
  Result.Result $ \freeLocals warnings bad good ->
    k () warnings
      (\() ws es -> bad freeLocals ws es)
      (\() ws (value, newFreeLocals) ->
          let delayedLocals = Map.map delayUse newFreeLocals in
          good (Map.unionWith combineUses freeLocals delayedLocals) ws value
      )



-- FIND VARIABLE


findVar :: R.Region -> Env.Env -> N.Name -> Result FreeLocals w Can.Expr_
findVar region (Env.Env localHome vs _ _ _ qvs _ _) name =
  case Map.lookup name vs of
    Just var ->
      case var of
        Env.Local _ ->
          logVar name (Can.VarLocal name)

        Env.TopLevel _ ->
          logVar name (Can.VarTopLevel localHome name)

        Env.Foreign (I.Bin 1 home annotation _ _) ->
          Result.ok $
            if home == ModuleName.debug then
              Can.VarDebug localHome name annotation
            else
              Can.VarForeign home name annotation

        Env.Foreign homes ->
          Result.throw (Error.AmbiguousVar region Nothing name (Map.keys homes))

    Nothing ->
      Result.throw (Error.NotFoundVar region Nothing name (toPossibleNames vs qvs))


findVarQual :: R.Region -> Env.Env -> N.Name -> N.Name -> Result FreeLocals w Can.Expr_
findVarQual region (Env.Env localHome vs _ _ _ qvs _ _) prefix name =
  case Map.lookup prefix qvs of
    Just qualified ->
      case Map.lookup name qualified of
        Just (I.Bin 1 home annotation _ _) ->
          Result.ok $
            if home == ModuleName.debug then
              Can.VarDebug localHome name annotation
            else
              Can.VarForeign home name annotation

        Just homes ->
          Result.throw (Error.AmbiguousVar region (Just prefix) name (Map.keys homes))

        Nothing ->
          Result.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames vs qvs))

    Nothing ->
      if ModuleName.isKernel prefix then
        Result.ok $ Can.VarKernel (ModuleName.getKernel prefix) name
      else
        Result.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames vs qvs))


toPossibleNames :: Map.Map N.Name Env.Var -> Env.Qualified Can.Annotation -> Error.PossibleNames
toPossibleNames exposed qualified =
  Error.PossibleNames (Map.keysSet exposed) (Map.map Map.keysSet qualified)



-- FIND CTOR


toVarCtor :: N.Name -> Env.Ctor -> Can.Expr_
toVarCtor name ctor =
  case ctor of
    Env.Ctor home typeName (Can.Union vars _ _ opts) index args ->
      let
        freeVars = Map.fromList (map (\v -> (v, ())) vars)
        result = Can.TType home typeName (map Can.TVar vars)
        tipe = foldr Can.TLambda result args
      in
      Can.VarCtor opts home name index (Can.Forall freeVars tipe)

    Env.RecordCtor home vars tipe ->
      let
        freeVars = Map.fromList (map (\v -> (v, ())) vars)
      in
      Can.VarCtor Can.Normal home name Index.first (Can.Forall freeVars tipe)
