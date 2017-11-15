{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Expression
  ( canonicalize
  , removeLocals
  , gatherTypedArgs
  )
  where


import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Type as Type
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

    Valid.Lambda srcArgs body ->
      addLocals $
      do  (args, destructors) <-
            Pattern.verify Error.DPLambdaArgs $
              Index.indexedTraverse (Pattern.canonicalizeArg env) srcArgs
          newEnv <- Env.addLocals destructors env
          removeLocals Warning.Pattern destructors $
            Can.Lambda args destructors <$> canonicalize newEnv body

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
        <*> traverse (canonicalizeCaseBranch env) branches

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


canonicalizeCaseBranch :: Env.Env -> (Src.Pattern, Valid.Expr) -> Result FreeLocals Can.CaseBranch
canonicalizeCaseBranch env (pattern, expr) =
  addLocals $
  do  (cpattern, destructors) <-
        Pattern.verify Error.DPCaseBranch $
          Pattern.canonicalize env Index.first pattern
      newEnv <- Env.addLocals destructors env
      removeLocals Warning.Pattern destructors $
        Can.CaseBranch cpattern destructors <$> canonicalize newEnv expr



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
canonicalizeLet letRegion env defs body =
  addLocals $
  do  let keyedDefs = zip defs [ 0 .. length defs ]
      let toError name () () = Error.DuplicatePattern Error.DPLetBinding name
      let namesBag = foldr addBindings Bag.empty keyedDefs
      boundNames <- Dups.detect toError (Bag.toList namesBag)
      newEnv <- Env.addLocals boundNames env
      let defKeys = Map.map A.drop boundNames
      nodes <- traverse (defToNode defKeys newEnv) keyedDefs
      removeLocals Warning.Binding boundNames $
        do  cbody <- canonicalize env body
            detectCycles letRegion (Graph.stronglyConnComp nodes) cbody


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



-- ADD BINDINGS


type KeyBag =
  Bag.Bag ( N.Name, [Dups.Info () (A.Located Int)] )


addBindings :: (Valid.Def, Int) -> KeyBag -> KeyBag
addBindings (def, key) bag =
  case def of
    Valid.Define defRegion (A.A region name) _ _ _ ->
      Bag.insert (Dups.info name region () (A.A defRegion key)) bag

    Valid.Destruct defRegion pattern _ ->
      addBindingsHelp (A.A defRegion key) pattern bag


addBindingsHelp :: A.Located Int -> Src.Pattern -> KeyBag -> KeyBag
addBindingsHelp key (A.A region pattern) bag =
  case pattern of
    Src.PAnything ->
      bag

    Src.PVar name ->
      Bag.insert (Dups.info name region () key) bag

    Src.PRecord fields ->
      let
        fieldToInfo (A.A fieldRegion name) =
          Dups.info name fieldRegion () key
      in
      foldr (\f b -> Bag.insert (fieldToInfo f) b) bag fields

    Src.PUnit ->
      bag

    Src.PTuple a b cs ->
      foldr (addBindingsHelp key) bag (a:b:cs)

    Src.PCtor _ _ _ patterns ->
      foldr (addBindingsHelp key) bag patterns

    Src.PList patterns ->
      foldr (addBindingsHelp key) bag patterns

    Src.PCons first rest ->
      addBindingsHelp key rest (addBindingsHelp key first bag)

    Src.PAlias ptrn (A.A reg name) ->
      addBindingsHelp key ptrn (Bag.insert (Dups.info name reg () key) bag)

    Src.PChr _ ->
      bag

    Src.PStr _ ->
      bag

    Src.PInt _ ->
      bag



-- BUILD BINDINGS GRAPH


data Node = Node Binding FreeLocals


data Binding
  = Define R.Region Can.Def
  | Destruct R.Region Can.Pattern Can.Destructors Can.Expr


defToNode :: Map.Map N.Name Int -> Env.Env -> (Valid.Def, Int) -> Result () (Node, Int, [Int])
defToNode defKeys env (def, key) =
  case def of
    Valid.Define region aname@(A.A _ name) srcArgs body Nothing ->
      do  (args, destructors) <-
            Pattern.verify (Error.DPFuncArgs name) $
              Index.indexedTraverse (Pattern.canonicalizeArg env) srcArgs
          newEnv <- Env.addLocals destructors env
          toNode defKeys key destructors $
            do  cbody <- canonicalize newEnv body
                return $ Define region $
                  Can.Def aname args destructors cbody

    Valid.Destruct region pattern body ->
      do  (cpattern, destructors) <-
            Pattern.verify Error.DPDestruct $
              Pattern.canonicalize env Index.first pattern

          toNode defKeys key destructors $
            Destruct region cpattern destructors <$> canonicalize env body

    Valid.Define region aname@(A.A _ name) srcArgs body (Just tipe) ->
      do  (Can.Forall freeVars ctipe) <- Type.toAnnotation env tipe
          ((args, resultType), destructors) <-
            Pattern.verify (Error.DPFuncArgs name) $
              gatherTypedArgs env name srcArgs ctipe Index.first []
          newEnv <- Env.addLocals destructors env
          toNode defKeys key destructors $
            do  cbody <- canonicalize newEnv body
                return $ Define region $
                  Can.TypedDef aname freeVars args destructors cbody resultType


toNode :: Map.Map N.Name Int -> Int -> Map.Map N.Name a -> Result FreeLocals Binding -> Result () (Node, Int, [Int])
toNode defKeys key args (Result.Result freeLocals warnings answer) =
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
        , key
        , Map.elems locallyDefinedLocals
        )



-- GATHER TYPED ARGS


gatherTypedArgs :: Env.Env -> N.Name -> [Src.Pattern] -> Can.Type -> Index.ZeroBased -> [Can.TypedArg] -> Result Pattern.Bag ([Can.TypedArg], Can.Type)
gatherTypedArgs env name srcArgs tipe index revTypedArgs =
  case srcArgs of
    [] ->
      return (reverse revTypedArgs, tipe)

    srcArg : otherSrcArgs ->
      case Type.iteratedDealias tipe of
        Can.TLambda argType resultType ->
          do  typedArg <- Can.TypedArg index argType
                <$> Pattern.canonicalize env index srcArg
              gatherTypedArgs env name otherSrcArgs resultType (Index.next index) $
                typedArg : revTypedArgs

        _ ->
          let (A.A start _, A.A end _) = (head srcArgs, last srcArgs) in
          Result.throw (R.merge start end) $
            Error.AnnotationTooShort name index (length srcArgs)



-- DETECT CYCLES


detectCycles :: R.Region -> [Graph.SCC Node] -> Can.Expr -> Result FreeLocals Can.Expr
detectCycles letRegion sccs body =
  case sccs of
    [] ->
      Result.ok body

    scc : subSccs ->
      A.A letRegion <$>
      case scc of
        Graph.AcyclicSCC (Node binding freeLocals) ->
          case binding of
            Define _ def ->
              Result.accumulate freeLocals (Can.Let def)
                <*> detectCycles letRegion subSccs body

            Destruct _ pattern destructors expr ->
              Result.accumulate freeLocals (Can.LetDestruct pattern destructors expr)
                <*> detectCycles letRegion subSccs body

        Graph.CyclicSCC nodes ->
          case unzip <$> traverse requireFunction nodes of
            Nothing ->
              Result.throw letRegion (Error.RecursiveLet (map toCycleNodes nodes))

            Just (defs, freeLocals) ->
              Result.accumulate (Set.unions freeLocals) (Can.LetRec defs)
                <*> detectCycles letRegion subSccs body


requireFunction :: Node -> Maybe (Can.Def, FreeLocals)
requireFunction (Node binding freeLocals) =
  case binding of
    Define _ def@(Can.Def _ (_:_) _ _) ->
      Just (def, freeLocals)

    Define _ def@(Can.TypedDef _ _ (_:_) _ _ _) ->
      Just (def, freeLocals)

    _ ->
      Nothing


toCycleNodes :: Node -> Error.CycleNode
toCycleNodes (Node binding _) =
  case binding of
    Define _ def ->
      case def of
        Can.Def name args _ _ ->
          if null args then Error.CycleValue name else Error.CycleFunc name

        Can.TypedDef name _ args _ _ _ ->
          if null args then Error.CycleValue name else Error.CycleFunc name

    Destruct _ pattern _ _ ->
      Error.CyclePattern pattern
