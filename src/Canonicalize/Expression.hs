{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Expression
  ( canonicalize
  , removeLocals
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified AST.Utils.Binop as Binop
import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULTS


type Result i a =
  Result.Result i Warning.Warning Error.Error a


type FreeLocals =
  Set.Set N.Name



-- EXPRESSIONS


canonicalize :: Env.Env -> Valid.Expr -> Result FreeLocals Can.Expr
canonicalize env (A.A region expression) =
  A.A region <$>
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
      Env.findVar region env maybePrefix name

    Valid.List exprs ->
      Can.List <$> traverse (canonicalize env) exprs

    Valid.Op op ->
      do  (Env.Binop _ home name annotation _ _) <- Env.findBinop region env op
          return (Can.VarOperator op home name annotation)

    Valid.Negate expr ->
      Can.Negate <$> canonicalize env expr

    Valid.Binops ops final ->
      A.drop <$> canonicalizeBinops region env ops final

    Valid.Lambda args body ->
      addLocals $
      do  cargs@(Can.Args _ destructors) <- Pattern.canonicalizeArgs env args
          newEnv <- Env.addLocals destructors env
          removeLocals Warning.Pattern destructors $
            Can.Lambda cargs <$> canonicalize newEnv body

    Valid.Call func args ->
      Can.Call
        <$> canonicalize env func
        <*> traverse (canonicalize env) args

    Valid.If branches finally ->
      Can.If
        <$> traverse (canonicalizeIfBranch env) branches
        <*> canonicalize env finally

    Valid.Let defs expr ->
      A.drop <$> canonicalizeLet region env defs expr

    Valid.Case expr branches ->
      Can.Case
        <$> canonicalize env expr
        <*> Index.indexedTraverse (canonicalizeCaseBranch env) branches

    Valid.Accessor field ->
      Result.ok $ Can.Accessor field

    Valid.Access record field ->
      Can.Access
        <$> canonicalize env record
        <*> Result.ok field

    Valid.Update (A.A reg name) fields ->
      do  fieldDict <- Result.untracked $ Dups.checkFields fields
          name_ <- Env.findVar reg env Nothing name
          Can.Update (A.A reg name_)
            <$> traverse (canonicalize env) fieldDict

    Valid.Record fields ->
      do  fieldDict <- Result.untracked $ Dups.checkFields fields
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


canonicalizeTupleExtras :: R.Region -> Env.Env -> [Valid.Expr] -> Result FreeLocals (Maybe Can.Expr)
canonicalizeTupleExtras region env extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> canonicalize env three

    _ : others ->
      let (A.A r1 _, A.A r2 _) = (head others, last others) in
      Result.throw region (Error.TupleLargerThanThree (R.merge r1 r2))



-- CANONICALIZE IF BRANCH


canonicalizeIfBranch :: Env.Env -> (Valid.Expr, Valid.Expr) -> Result FreeLocals (Can.Expr, Can.Expr)
canonicalizeIfBranch env (condition, branch) =
  (,)
    <$> canonicalize env condition
    <*> canonicalize env branch



-- CANONICALIZE CASE BRANCH


canonicalizeCaseBranch :: Env.Env -> Index.ZeroBased -> (Src.Pattern, Valid.Expr) -> Result FreeLocals Can.CaseBranch
canonicalizeCaseBranch env index (pattern, expr) =
  addLocals $
  do  (cpattern, destructors) <- Pattern.canonicalize env index pattern
      newEnv <- Env.addLocals destructors env
      removeLocals Warning.Pattern destructors $
        Can.CaseBranch index cpattern destructors <$> canonicalize newEnv expr



-- CANONICALIZE BINOPS


canonicalizeBinops :: R.Region -> Env.Env -> [(Valid.Expr, A.Located N.Name)] -> Valid.Expr -> Result FreeLocals Can.Expr
canonicalizeBinops overallRegion env ops final =
  let
    canonicalizeHelp (expr, A.A region op) =
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


runBinopStepper :: R.Region -> Step -> Result FreeLocals Can.Expr
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
      Result.throw overallRegion (Error.Binop op1 op2)


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


canonicalizeLet :: R.Region -> Env.Env -> [Valid.Def] -> Valid.Expr -> Result FreeLocals Can.Expr
canonicalizeLet region env defs body =
  addLocals $
  do  (bindings, boundNames) <- Pattern.canonicalizeBindings env defs
      newEnv <- Env.addLocals boundNames env
      let defKeys = Map.map A.drop boundNames
      nodes <- Result.untracked $ traverse (bindingToNode defKeys newEnv) bindings
      removeLocals Warning.Binding boundNames $
        do  cbody <- canonicalize env body
            detectCycles region (Graph.stronglyConnComp nodes) cbody


addLocals :: Result () (expr, FreeLocals) -> Result FreeLocals expr
addLocals (Result.Result () warnings answer) =
  case answer of
    Result.Ok (value, freeLocals) ->
      Result.Result freeLocals warnings (Result.Ok value)

    Result.Err err ->
      Result.Result Set.empty warnings (Result.Err err)


removeLocals :: Warning.Unused -> Map.Map N.Name (A.Located a) -> Result FreeLocals expr -> Result () (expr, FreeLocals)
removeLocals unused boundNames (Result.Result freeLocals warnings answer) =
  case answer of
    Result.Err err ->
      Result.Result () warnings (Result.Err err)

    Result.Ok value ->
      let
        newFreeLocals =
          Set.difference freeLocals (Map.keysSet boundNames)

        newWarnings =
          -- NOTE: check for unused variables without any large allocations.
          -- This relies on the fact that Map.size and Set.size are O(1)
          if Set.size freeLocals - Map.size boundNames == Set.size newFreeLocals then
            warnings
          else
            Bag.append warnings $ Bag.fromList (toUnusedWarning unused) $
              Map.toList (Map.withoutKeys boundNames freeLocals)
      in
      Result.Result () newWarnings (Result.Ok (value, newFreeLocals))


toUnusedWarning :: Warning.Unused -> (N.Name, A.Located a) -> A.Located Warning.Warning
toUnusedWarning unused (name, A.A region _) =
  A.A region (Warning.UnusedVariable unused name)



-- BUILD BINDINGS GRAPH


data Binding
  = Define R.Region Can.Def
  | Destruct R.Region Can.Pattern Can.Destructors Can.Expr


data Node = Node Binding FreeLocals


bindingToNode :: Map.Map N.Name Int -> Env.Env -> Pattern.Binding -> Result () (Node, Int, [Int])
bindingToNode defKeys env binding =
  case binding of
    Pattern.Define region index name args body maybeType ->
      do  cargs@(Can.Args _ destructors) <- Pattern.canonicalizeArgs env args
          maybeAnnotation <- traverse (Type.toAnnotation env) maybeType
          newEnv <- Env.addLocals destructors env
          toNode defKeys destructors index $
            do  cbody <- canonicalize newEnv body
                return $ Define region $ Can.Def name cargs cbody maybeAnnotation

    Pattern.Destruct region index pattern destructors body ->
      toNode defKeys Map.empty index $
        Destruct region pattern destructors <$> canonicalize env body


toNode :: Map.Map N.Name Int -> Map.Map N.Name a -> Int -> Result FreeLocals Binding -> Result () (Node, Int, [Int])
toNode defKeys args index (Result.Result freeLocals warnings answer) =
  Result.Result () warnings $
  case answer of
    Result.Err err ->
      Result.Err err

    Result.Ok binding ->
      let
        actuallyFreeLocals = Set.difference freeLocals (Map.keysSet args)
        locallyDefinedLocals = Map.restrictKeys defKeys freeLocals
      in
      Result.Ok
        ( Node binding actuallyFreeLocals
        , index
        , Map.elems locallyDefinedLocals
        )



-- DETECT CYCLES


detectCycles :: R.Region -> [Graph.SCC Node] -> Can.Expr -> Result FreeLocals Can.Expr
detectCycles region sccs body =
  case sccs of
    [] ->
      Result.ok body

    scc : subSccs ->
      A.A region <$>
      case scc of
        Graph.AcyclicSCC (Node binding freeLocals) ->
          case binding of
            Define _ def ->
              Result.accumulate freeLocals (Can.Let def)
                <*> detectCycles region subSccs body

            Destruct _ pattern destructors expr ->
              Result.accumulate freeLocals (Can.LetDestruct pattern destructors expr)
                <*> detectCycles region subSccs body

        Graph.CyclicSCC nodes ->
          case unzip <$> traverse requireDefine nodes of
            Nothing ->
              Result.throw region (Error.RecursiveLet (map toCycleNodes nodes))

            Just (defs, freeLocals) ->
              Result.accumulate (Set.unions freeLocals) (Can.LetRec defs)
                <*> detectCycles region subSccs body



requireDefine :: Node -> Maybe (Can.Def, FreeLocals)
requireDefine (Node binding freeLocals) =
  case binding of
    Define _ def@(Can.Def _ (Can.Args args _) _ _) ->
      case args of
        [] ->
          Nothing

        _ ->
          Just (def, freeLocals)

    Destruct _ _ _ _ ->
      Nothing


toCycleNodes :: Node -> Error.CycleNode
toCycleNodes (Node binding _) =
  case binding of
    Define _ (Can.Def name (Can.Args args _) _ _) ->
      case args of
        [] ->
          Error.CycleValue name

        _ ->
          Error.CycleFunc name

    Destruct _ pattern _ _ ->
      Error.CyclePattern pattern
