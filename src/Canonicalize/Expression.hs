{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Expression
  ( canonicalize
  , FreeLocals
  , Uses(..)
  , verifyBindings
  , gatherTypedArgs
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map

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
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Data.OneOrMore as OneOrMore
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

    Valid.Var maybePrefix name ->
      findVar region env maybePrefix name

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
      do  fieldDict <- Dups.checkFields fields
          name_ <- findVar reg env Nothing name
          Can.Update (A.At reg name_)
            <$> traverse (canonicalize env) fieldDict

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
  do  let keyedDefs = zip defs [ 0 .. length defs ]
      let names = foldr addBindings Dups.none keyedDefs
      nameDict <- Dups.detect (Error.DuplicatePattern Error.DPLetBinding) names

      let bindings = Map.map A.toRegion nameDict
      let defKeys = Map.map A.toValue nameDict
      newEnv <- Env.addLocals bindings env

      ((nodes, cbody), freeLocals) <-
        verifyBindings W.Def bindings $
          (,)
            <$> traverse (defToNode defKeys newEnv) keyedDefs
            <*> canonicalize newEnv body

      letExpr <-
        detectCycles letRegion (Graph.stronglyConnComp nodes) cbody

      return (letExpr, freeLocals)



-- ADD BINDINGS


type Bindings =
  Dups.Dict (A.Located Int)


addBindings :: (Valid.Def, Int) -> Bindings -> Bindings
addBindings (def, key) bindings =
  case def of
    Valid.Define defRegion (A.At region name) _ _ _ ->
      Dups.insert name region (A.At defRegion key) bindings

    Valid.Destruct defRegion pattern _ ->
      addBindingsHelp (A.At defRegion key) pattern bindings


addBindingsHelp :: A.Located Int -> Src.Pattern -> Bindings -> Bindings
addBindingsHelp key (A.At region pattern) bindings =
  case pattern of
    Src.PAnything ->
      bindings

    Src.PVar name ->
      Dups.insert name region key bindings

    Src.PRecord fields ->
      let
        addField (A.At fieldRegion name) dict =
          Dups.insert name fieldRegion key dict
      in
      foldr addField bindings fields

    Src.PUnit ->
      bindings

    Src.PTuple a b cs ->
      foldr (addBindingsHelp key) bindings (a:b:cs)

    Src.PCtor _ _ _ patterns ->
      foldr (addBindingsHelp key) bindings patterns

    Src.PList patterns ->
      foldr (addBindingsHelp key) bindings patterns

    Src.PCons first rest ->
      addBindingsHelp key rest (addBindingsHelp key first bindings)

    Src.PAlias ptrn (A.At reg name) ->
      addBindingsHelp key ptrn (Dups.insert name reg key bindings)

    Src.PChr _ ->
      bindings

    Src.PStr _ ->
      bindings

    Src.PInt _ ->
      bindings



-- BUILD BINDINGS GRAPH


type Node =
  (Binding, Int, [Int])


data Binding
  = Define R.Region Can.Def
  | Destruct R.Region Can.Pattern Can.Expr


defToNode :: Map.Map N.Name Int -> Env.Env -> (Valid.Def, Int) -> Result FreeLocals [W.Warning] Node
defToNode defKeys env (def, key) =
  case def of
    Valid.Define region aname@(A.At _ name) srcArgs body Nothing ->
      do  (args, argBindings) <-
            Pattern.verify (Error.DPFuncArgs name) $
              traverse (Pattern.canonicalize env) srcArgs

          newEnv <-
            Env.addLocals argBindings env

          (cbody, freeLocals) <-
            verifyBindings W.Pattern argBindings (canonicalize newEnv body)

          logLetLocals args freeLocals
            ( Define region (Can.Def aname args cbody)
            , key
            , Map.elems (Map.intersection defKeys freeLocals)
            )

    Valid.Destruct region pattern body ->
      do  (cpattern, _bindings) <-
            Pattern.verify Error.DPDestruct $
              Pattern.canonicalize env pattern

          Result.Result $ \freeLocals warnings bad good ->
            case canonicalize env body of
              Result.Result k ->
                k Map.empty warnings
                  (\_ ws es -> bad freeLocals ws es)
                  (\newFreeLocals warnings1 cbody ->
                      good
                        (Map.unionWith combineUses freeLocals newFreeLocals)
                        warnings1
                        ( Destruct region cpattern cbody
                        , key
                        , Map.elems (Map.intersection defKeys freeLocals)
                        )
                  )

    Valid.Define region aname@(A.At _ name) srcArgs body (Just tipe) ->
      do  (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
          ((args, resultType), argBindings) <-
            Pattern.verify (Error.DPFuncArgs name) $
              gatherTypedArgs env name srcArgs ctipe Index.first []

          newEnv <-
            Env.addLocals argBindings env

          (cbody, freeLocals) <-
            verifyBindings W.Pattern argBindings (canonicalize newEnv body)

          logLetLocals args freeLocals
            ( Define region (Can.TypedDef aname freeVars args cbody resultType)
            , key
            , Map.elems (Map.intersection defKeys freeLocals)
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
      A.At letRegion <$>
      case scc of
        Graph.AcyclicSCC binding ->
          case binding of
            Define _ def ->
              Can.Let def
                <$> detectCycles letRegion subSccs body

            Destruct _ pattern expr ->
              Can.LetDestruct pattern expr
                <$> detectCycles letRegion subSccs body

        Graph.CyclicSCC bindings ->
          case traverse requireFunction bindings of
            Just defs ->
              Can.LetRec defs
                <$> detectCycles letRegion subSccs body

            Nothing ->
              Result.throw (Error.RecursiveLet letRegion (map toCycleNodes bindings))


requireFunction :: Binding -> Maybe Can.Def
requireFunction binding =
  case binding of
    Define _ def@(Can.Def _ (_:_) _) ->
      Just def

    Define _ def@(Can.TypedDef _ _ (_:_) _ _) ->
      Just def

    _ ->
      Nothing


toCycleNodes :: Binding -> Error.CycleNode
toCycleNodes binding =
  case binding of
    Define _ def ->
      case def of
        Can.Def name args _ ->
          if null args then Error.CycleValue name else Error.CycleFunc name

        Can.TypedDef name _ args _ _ ->
          if null args then Error.CycleValue name else Error.CycleFunc name

    Destruct _ pattern _ ->
      Error.CyclePattern pattern



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


findVar :: R.Region -> Env.Env -> Maybe N.Name -> N.Name -> Result FreeLocals w Can.Expr_
findVar region (Env.Env localHome vars _ _ _) maybePrefix name =
  case Map.lookup name vars of
    Nothing ->
      case maybePrefix of
        Nothing ->
          Result.throw (error "TODO no name" region)

        Just prefix ->
          if ModuleName.isKernel prefix then
            Result.ok $ Can.VarKernel (ModuleName.getKernel prefix) name
          else
            Result.throw (error "TODO no name" region)

    Just (Env.VarHomes unqualified qualified) ->
      case maybePrefix of
        Nothing ->
          case unqualified of
            Env.Local _ ->
              logVar name (Can.VarLocal name)

            Env.TopLevel _ ->
              logVar name (Can.VarTopLevel localHome name)

            Env.Foreign bag ->
              case bag of
                Bag.One (Env.ForeignVarHome home annotation) ->
                  Result.ok $
                    if home == ModuleName.debug then
                      Can.VarDebug localHome name annotation
                    else
                      Can.VarForeign home name annotation

                Bag.Empty ->
                  Result.throw (error "TODO no unqualified" region)

                Bag.Two _ _ ->
                  Result.throw (error "TODO ambiguous unqualified" region)

        Just prefix ->
          case Map.lookup prefix qualified of
            Nothing ->
              Result.throw (error "TODO no qualified" region)

            Just (OneOrMore.One (Env.ForeignVarHome home annotation)) ->
              Result.ok $
                if home == ModuleName.debug then
                  Can.VarDebug localHome name annotation
                else
                  Can.VarForeign home name annotation

            Just _ ->
              Result.throw (error "TODO ambiguous qualified" region)
