{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Sort (definitions) where

import qualified Data.Foldable as F
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Expression.Canonical as C
import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Result as R



-- SORT DEFINITIONS


definitions :: (Monoid i) => C.Expr -> R.Result i w E.Error C.Expr
definitions expression =
  let
    (R.Result _ warnings answer) =
      reorder expression
  in
    R.Result mempty warnings answer


data LocalVars =
  LocalVars
    { _immediate :: Set.Set Text
    , _delayed :: Set.Set Text
    , _tags :: Set.Set Text
    }


instance Monoid LocalVars where
  mempty =
    LocalVars Set.empty Set.empty Set.empty

  mappend (LocalVars i1 d1 t1) (LocalVars i2 d2 t2) =
    LocalVars (Set.union i1 i2) (Set.union d1 d2) (Set.union t1 t2)


addTag :: Text -> a -> R.Result LocalVars w e a
addTag name value =
  R.accumulate (LocalVars Set.empty Set.empty (Set.singleton name)) value


addImmediate :: V.Canonical -> a -> R.Result LocalVars w e a
addImmediate (V.Canonical home name) value =
  case home of
    V.Local ->
      R.accumulate (LocalVars (Set.singleton name) Set.empty Set.empty) value

    V.TopLevel _ ->
      R.accumulate (LocalVars (Set.singleton name) Set.empty Set.empty) value

    V.BuiltIn ->
      return value

    V.Module _ ->
      return value


capture :: P.Canonical -> R.Result LocalVars w e a -> R.Result LocalVars w e a
capture pattern result =
  captureVars (P.boundVarSet pattern) result


captureVars :: Set.Set Text -> R.Result LocalVars w e a -> R.Result LocalVars w e a
captureVars bound (R.Result (LocalVars immediate delayed tags) warnings result) =
  let
    localVars =
      LocalVars (Set.difference immediate bound) (Set.difference delayed bound) tags
  in
    R.Result localVars warnings result


delay :: R.Result LocalVars w e a -> R.Result LocalVars w e a
delay (R.Result (LocalVars immediate delayed tags) warnings result) =
  R.Result (LocalVars Set.empty (Set.union immediate delayed) tags) warnings result



-- REORDER EXPRESSIONS


reorder :: C.Expr -> R.Result LocalVars w E.Error C.Expr
reorder (A.A region expression) =
  A.A region <$>
  case expression of
    C.Var var ->
      addImmediate var expression

    C.Lambda pattern body ->
      delay $
        bindingReorder C.Lambda (pattern, body)

    C.Binop op leftExpr rightExpr ->
      do  binop <- C.Binop op <$> reorder leftExpr <*> reorder rightExpr
          addImmediate op binop

    C.Case expr cases ->
      C.Case <$> reorder expr <*> traverse (bindingReorder (,)) cases

    C.Ctor ctor@(V.Canonical _ name) exprs ->
      addTag name =<< (C.Ctor ctor <$> traverse reorder exprs)

    -- Just pipe the reorder though
    C.Literal _ ->
      return expression

    C.List es ->
      C.List <$> traverse reorder es

    C.App func arg ->
      C.App <$> reorder func <*> reorder arg

    C.If branches finally ->
      C.If
        <$> traverse (\(cond,branch) -> (,) <$> reorder cond <*> reorder branch) branches
        <*> reorder finally

    C.Access record field ->
      C.Access
        <$> reorder record
        <*> return field

    C.Update record fields ->
      C.Update
        <$> reorder record
        <*> traverse (traverse reorder) fields

    C.Record fields ->
      C.Record
        <$> traverse (traverse reorder) fields

    C.Cmd _ ->
      return expression

    C.Sub _ ->
      return expression

    C.OutgoingPort _ _ ->
      return expression

    C.IncomingPort _ _ ->
      return expression

    C.Program _ _ ->
      error "DANGER - Program AST nodes should not be in def sorting."

    C.SaveEnv _ _ ->
      return expression

    C.GLShader _ _ _ ->
      return expression

    -- Actually do some reordering
    C.Let defs body ->
      case reorderDefs defs of
        R.Result () warnings (R.Err errors) ->
          R.Result mempty warnings (R.Err errors)

        R.Result () warnings (R.Ok (boundVars, defGroups, defLocals)) ->
          captureVars boundVars $
            do  _ <- R.Result mempty warnings (R.Ok ())
                newBody <- reorder body
                R.accumulate defLocals $ A.drop $
                  foldr (\defGroup expr -> A.A region (C.Let defGroup expr)) newBody defGroups



-- BINDINGS


bindingReorder
    :: (P.Canonical -> C.Expr -> a)
    -> (P.Canonical, C.Expr)
    -> R.Result LocalVars w E.Error a
bindingReorder func (pattern, expr) =
  do  answer <- func pattern <$> capture pattern (reorder expr)
      addPatternTags pattern answer



-- FREE CONSTRUCTORS


addPatternTags :: P.Canonical -> a -> R.Result LocalVars w e a
addPatternTags pattern value =
  R.accumulate (LocalVars Set.empty Set.empty (getPatternTags pattern)) value


getPatternTags :: P.Canonical -> Set.Set Text
getPatternTags (A.A _ pattern) =
  case pattern of
    P.Var _ ->
      Set.empty

    P.Alias _ subPattern ->
      getPatternTags subPattern

    P.Record _ ->
      Set.empty

    P.Anything ->
      Set.empty

    P.Literal _ ->
      Set.empty

    P.Ctor (V.Canonical home name) patterns ->
      let
        freeCtors =
          Set.unions (map getPatternTags patterns)
      in
        case home of
          V.Local ->
            Set.insert name freeCtors

          V.TopLevel _ ->
            Set.insert name freeCtors

          V.BuiltIn ->
            freeCtors

          V.Module _ ->
            freeCtors



-- REORDER DEFINITIONS


reorderDefs
  :: [C.Def]
  -> R.Result () w E.Error (Set.Set Text, [[C.Def]], LocalVars)
reorderDefs defs =
  do  rawNodes <- sequenceA $ zipWith toRawNode [0..] defs
      let boundVarDict = toBoundVarDict rawNodes
      _ <- detectLoops boundVarDict rawNodes
      return $ groupDefs boundVarDict rawNodes


toRawNode :: Int -> C.Def -> R.Result () w E.Error (C.Def, Int, LocalVars)
toRawNode index (C.Def region pattern body maybeType) =
  let
    (R.Result localVars warnings answer) =
      addPatternTags pattern =<< reorder body

    toRawNodeHelp newBody =
      ( C.Def region pattern newBody maybeType, index, localVars )
  in
    toRawNodeHelp <$> R.Result () warnings answer


toBoundVarDict :: [(C.Def, Int, a)] -> Map.Map Text Int
toBoundVarDict defAndDeps =
  Map.fromList (concatMap toBoundVarDictHelp defAndDeps)


toBoundVarDictHelp :: (C.Def, Int, a) -> [(Text, Int)]
toBoundVarDictHelp (C.Def _ pattern _ _, key, _) =
  map (\(A.A _ name) -> (name, key)) (P.boundVars pattern)



-- BUILD GRAPHS


toNode
  :: (deps -> [Text])
  -> Map.Map Text Int
  -> (C.Def, Int, deps)
  -> (C.Def, Int, [Int])
toNode toVars boundVarDict (def, index, deps) =
  let
    localDeps =
      Maybe.mapMaybe (\var -> Map.lookup var boundVarDict) (toVars deps)
  in
    ( def, index, localDeps )


immediateOnly :: LocalVars -> [Text]
immediateOnly (LocalVars immediate _ _) =
  Set.toList immediate


allVars :: LocalVars -> [Text]
allVars (LocalVars immediate delayed tags) =
  Set.toList immediate ++ Set.toList delayed ++ Set.toList tags



-- DETECT LOOPS


detectLoops
  :: Map.Map Text Int
  -> [(C.Def, Int, LocalVars)]
  -> R.Result () w E.Error ()
detectLoops boundVarDict rawNodes =
  F.traverse_ detectLoopsHelp $
    Graph.stronglyConnComp $
      map (toNode immediateOnly boundVarDict) rawNodes


detectLoopsHelp :: Graph.SCC C.Def -> R.Result () w E.Error ()
detectLoopsHelp scc =
  case scc of
    Graph.AcyclicSCC _ ->
      R.ok ()

    Graph.CyclicSCC defs ->
      case defs of
        [] ->
          R.ok ()

        def@(C.Def region (A.A nameRegion _) _ _) : rest ->
          R.throw region $ E.BadRecursion nameRegion def rest



-- GROUP DEFINITIONS


groupDefs
  :: Map.Map Text Int
  -> [(C.Def, Int, LocalVars)]
  -> ( Set.Set Text, [[C.Def]], LocalVars )
groupDefs boundVarDict rawNodes =
  let
    allLocalVars =
      mconcat (map (\(_, _, localVars) -> localVars) rawNodes)

    defGroups =
      map Graph.flattenSCC $
        Graph.stronglyConnComp $
          map (toNode allVars boundVarDict) rawNodes
  in
    ( Map.keysSet boundVarDict, defGroups, allLocalVars )
