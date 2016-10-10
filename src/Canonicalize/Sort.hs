{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Sort (definitions) where

import qualified Data.Foldable as F
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import AST.Expression.General (Expr'(..))
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Result as R



-- SORT DEFINITIONS


definitions :: (Monoid i) => Canonical.Expr -> R.Result i w E.Error Canonical.Expr
definitions expression =
  let
    (R.Result _ warnings answer) =
      reorder expression
  in
    R.Result mempty warnings answer


data LocalVars =
  LocalVars
    { _immediate :: Set.Set String
    , _delayed :: Set.Set String
    , _tags :: Set.Set String
    }


instance Monoid LocalVars where
  mempty =
    LocalVars Set.empty Set.empty Set.empty

  mappend (LocalVars i1 d1 t1) (LocalVars i2 d2 t2) =
    LocalVars (Set.union i1 i2) (Set.union d1 d2) (Set.union t1 t2)


addTag :: String -> a -> R.Result LocalVars w e a
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


captureVars :: Set.Set String -> R.Result LocalVars w e a -> R.Result LocalVars w e a
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


reorder :: Canonical.Expr -> R.Result LocalVars w E.Error Canonical.Expr
reorder (A.A region expression) =
  A.A region <$>
  case expression of
    Var var ->
      addImmediate var expression

    Lambda pattern body ->
      delay $
        bindingReorder Lambda (pattern, body)

    Binop op leftExpr rightExpr ->
      do  binop <- Binop op <$> reorder leftExpr <*> reorder rightExpr
          addImmediate op binop

    Case expr cases ->
      Case <$> reorder expr <*> traverse (bindingReorder (,)) cases

    Data name exprs ->
      addTag name =<< (Data name <$> traverse reorder exprs)

    -- Just pipe the reorder though
    Literal _ ->
      return expression

    ExplicitList es ->
      ExplicitList <$> traverse reorder es

    App func arg ->
      App <$> reorder func <*> reorder arg

    If branches finally ->
      If
        <$> traverse (\(cond,branch) -> (,) <$> reorder cond <*> reorder branch) branches
        <*> reorder finally

    Access record field ->
      Access
        <$> reorder record
        <*> return field

    Update record fields ->
      Update
        <$> reorder record
        <*> traverse (traverse reorder) fields

    Record fields ->
      Record
        <$> traverse (traverse reorder) fields

    Cmd _ ->
      return expression

    Sub _ ->
      return expression

    OutgoingPort _ _ ->
      return expression

    IncomingPort _ _ ->
      return expression

    Program _ _ ->
      error "DANGER - Program AST nodes should not be in def sorting."

    SaveEnv _ _ ->
      return expression

    GLShader _ _ _ ->
      return expression

    -- Actually do some reordering
    Let defs body ->
      case reorderDefs defs of
        R.Result () warnings (R.Err errors) ->
          R.Result mempty warnings (R.Err errors)

        R.Result () warnings (R.Ok (boundVars, defGroups, defLocals)) ->
          captureVars boundVars $
            do  _ <- R.Result mempty warnings (R.Ok ())
                newBody <- reorder body
                R.accumulate defLocals $ A.drop $
                  foldr (\defGroup expr -> A.A region (Let defGroup expr)) newBody defGroups



-- BINDINGS


bindingReorder
    :: (P.Canonical -> Canonical.Expr -> a)
    -> (P.Canonical, Canonical.Expr)
    -> R.Result LocalVars w E.Error a
bindingReorder func (pattern, expr) =
  do  answer <- func pattern <$> capture pattern (reorder expr)
      addPatternTags pattern answer



-- FREE CONSTRUCTORS


addPatternTags :: P.Canonical -> a -> R.Result LocalVars w e a
addPatternTags pattern value =
  R.accumulate (LocalVars Set.empty Set.empty (getPatternTags pattern)) value


getPatternTags :: P.Canonical -> Set.Set String
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

    P.Data (V.Canonical home name) patterns ->
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
  :: [Canonical.Def]
  -> R.Result () w E.Error (Set.Set String, [[Canonical.Def]], LocalVars)
reorderDefs defs =
  do  rawNodes <- sequenceA $ zipWith toRawNode [0..] defs
      let boundVarDict = toBoundVarDict rawNodes
      _ <- detectLoops boundVarDict rawNodes
      return $ groupDefs boundVarDict rawNodes


toRawNode :: Int -> Canonical.Def -> R.Result () w E.Error (Canonical.Def, Int, LocalVars)
toRawNode index (Canonical.Def region pattern body maybeType) =
  let
    (R.Result localVars warnings answer) =
      addPatternTags pattern =<< reorder body

    toRawNodeHelp newBody =
      ( Canonical.Def region pattern newBody maybeType, index, localVars )
  in
    toRawNodeHelp <$> R.Result () warnings answer


toBoundVarDict :: [(Canonical.Def, Int, a)] -> Map.Map String Int
toBoundVarDict defAndDeps =
  Map.fromList (concatMap toBoundVarDictHelp defAndDeps)


toBoundVarDictHelp :: (Canonical.Def, Int, a) -> [(String, Int)]
toBoundVarDictHelp (Canonical.Def _ pattern _ _, key, _) =
  map (\(A.A _ name) -> (name, key)) (P.boundVars pattern)



-- BUILD GRAPHS


toNode
  :: (deps -> [String])
  -> Map.Map String Int
  -> (Canonical.Def, Int, deps)
  -> (Canonical.Def, Int, [Int])
toNode toVars boundVarDict (def, index, deps) =
  let
    localDeps =
      Maybe.mapMaybe (\var -> Map.lookup var boundVarDict) (toVars deps)
  in
    ( def, index, localDeps )


immediateOnly :: LocalVars -> [String]
immediateOnly (LocalVars immediate _ _) =
  Set.toList immediate


allVars :: LocalVars -> [String]
allVars (LocalVars immediate delayed tags) =
  Set.toList immediate ++ Set.toList delayed ++ Set.toList tags



-- DETECT LOOPS


detectLoops
  :: Map.Map String Int
  -> [(Canonical.Def, Int, LocalVars)]
  -> R.Result () w E.Error ()
detectLoops boundVarDict rawNodes =
  F.traverse_ detectLoopsHelp $
    Graph.stronglyConnComp $
      map (toNode immediateOnly boundVarDict) rawNodes


detectLoopsHelp :: Graph.SCC Canonical.Def -> R.Result () w E.Error ()
detectLoopsHelp scc =
  case scc of
    Graph.AcyclicSCC _ ->
      R.ok ()

    Graph.CyclicSCC defs ->
      case defs of
        [] ->
          R.ok ()

        def@(Canonical.Def region (A.A nameRegion _) _ _) : rest ->
          R.throw region $ E.BadRecursion nameRegion def rest



-- GROUP DEFINITIONS


groupDefs
  :: Map.Map String Int
  -> [(Canonical.Def, Int, LocalVars)]
  -> ( Set.Set String, [[Canonical.Def]], LocalVars )
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
