module Type.Constrain.Expression where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Arrow (second)
import Control.Applicative ((<$>),(<*>))
import qualified Control.Monad as Monad
import Control.Monad.State
import Data.Traversable (traverse)

import SourceSyntax.Location as Loc
import SourceSyntax.Pattern (Pattern(PVar))
import SourceSyntax.Expression
import qualified SourceSyntax.Type as SrcT
import Type.Type hiding (Descriptor(..))
import Type.Fragment
import qualified Type.Environment as Env
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern

{-- Testing section --}
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import Parse.Expression
import Parse.Helpers (iParse)
import Type.Solve (solve)
import qualified Type.State as TS

test str =
  case iParse expr "" str of
    Left err -> error $ "Parse error at " ++ show err
    Right expression -> do
      env <- Env.initialEnvironment
      var <- flexibleVar
      constraint <- constrain env expression (VarN var)
      (env,_,_,errors) <- execStateT (solve constraint) TS.initialState
      forM (Map.toList env) $ \(n,t) -> do
          pt <- extraPretty t
          print $ P.text n <+> P.text ":" <+> pt
      if null errors then return () else do
          putStrLn "\n"
          mapM_ print =<< sequence errors
{-- todo: remove testing code --}

constrain :: Env.Environment -> LExpr a b -> Type -> IO TypeConstraint
constrain env (L _ _ expr) tipe =
    let list t = TermN (App1 (Env.get env Env.builtin "[_]") t) in
    case expr of
      Literal lit -> return $ Literal.constrain env lit tipe

      Var name -> return $ name <? tipe

      Range lo hi ->
          exists $ \x -> do
            clo <- constrain env lo x
            chi <- constrain env hi x
            return $ CAnd [clo, chi, list x === tipe]

      ExplicitList exprs ->
          exists $ \x -> do
            cs <- mapM (\e -> constrain env e x) exprs
            return $ CAnd (list x === tipe : cs)

      Binop op e1 e2 ->
          exists $ \t1 ->
          exists $ \t2 -> do
            c1 <- constrain env e1 t1
            c2 <- constrain env e2 t2
            return $ CAnd [ c1, c2, op <? (t1 ==> t2 ==> tipe) ]

      Lambda p e ->
          exists $ \t1 ->
          exists $ \t2 -> do
            fragment <- Pattern.constrain env p t1
            c2 <- constrain env e t2
            let c = ex (vars fragment) (CLet [monoscheme (typeEnv fragment)]
                                             (typeConstraint fragment /\ c2 ))
            return $ c /\ tipe === (t1 ==> t2)

      App e1 e2 ->
          exists $ \t -> do
            c1 <- constrain env e1 (t ==> tipe)
            c2 <- constrain env e2 t
            return $ c1 /\ c2

      MultiIf branches -> CAnd <$> mapM constrain' branches
          where 
             bool = Env.get env Env.builtin "Bool"
             constrain' (b,e) = do
                  cb <- constrain env b bool
                  ce <- constrain env e tipe
                  return (cb /\ ce)

      Case e branches ->
          exists $ \t -> do
            ce <- constrain env e t
            let branch (p,e) = do
                  fragment <- Pattern.constrain env p t
                  c <- constrain env e tipe
                  return $ ex (vars fragment)
                              (CLet [monoscheme (typeEnv fragment)]
                                    (typeConstraint fragment /\ c))
            CAnd . (:) ce <$> mapM branch branches

      Data name exprs ->
          do pairs <- mapM pair exprs
             (ctipe, cs) <- Monad.foldM step (tipe,CTrue) (reverse pairs)
             return (cs /\ name <? ctipe)
          where
            pair e = do v <- flexibleVar -- needs an ex
                        return (e, VarN v)

            step (t,c) (e,x) = do
                c' <- constrain env e x
                return (x ==> t, c /\ c')

      Access e label ->
          exists $ \t ->
              constrain env e (record (Map.singleton label [tipe]) t)

      Remove e label ->
          exists $ \t ->
              constrain env e (record (Map.singleton label [t]) tipe)

      Insert e label value ->
          exists $ \tVal ->
          exists $ \tRec -> do
              cVal <- constrain env value tVal
              cRec <- constrain env e tRec
              let c = tipe === record (Map.singleton label [tVal]) tRec
              return (CAnd [cVal, cRec, c])

      Modify e fields ->
          exists $ \t -> do
              dummyFields <- Map.fromList <$> mapM dummyField fields
              cOld <- constrain env e (record dummyFields t)
              (fieldTypes, constraints) <- unzip <$> mapM field fields
              let cNew = tipe === record (Map.fromList fieldTypes) t
              return (CAnd (cOld : cNew : constraints))
          where
            dummyField (label, _) = do
                v <- flexibleVar -- needs an ex
                return (label, [VarN v])

            field (label, value) = do
                v <- flexibleVar -- needs an ex
                c <- ex [v] <$> constrain env value (VarN v)
                return ((label, [VarN v]), c)

      Record fields ->
          do (pairs, cs) <- unzip <$> mapM field fields
             let fieldTypes = Map.fromList (map (second (\t -> [VarN t])) pairs)
                 recordType = record fieldTypes (TermN EmptyRecord1)
             return $ ex (map snd pairs) (CAnd (tipe === recordType : cs))
          where
            field (name, body) = do
                v <- flexibleVar -- needs an ex
                c <- constrain env body (VarN v)
                return ((name, v), c)

      Markdown _ ->
          return $ tipe === Env.get env Env.builtin "Element"

      Let defs body ->
          do c <- constrain env body tipe
             (schemes, rqs, fqs, header, c2, c1) <-
                 Monad.foldM (constrainDef env)
                             ([], [], [], Map.empty, CTrue, CTrue)
                             (collapseDefs defs)
             return $ CLet schemes
                           (CLet [Scheme rqs fqs (CLet [monoscheme header] c2) header ]
                                 (c1 /\ c))


constrainDef env info (pattern, expr, maybeTipe) =
    let qs = [] -- should come from the def, but I'm not sure what would live there...
        (schemes, rigidQuantifiers, flexibleQuantifiers, headers, c2, c1) = info
    in
    case (pattern, maybeTipe) of
      (PVar name, Just tipe) ->
          do flexiVars <- mapM (\_ -> flexibleVar) qs
             let inserts = zipWith (\arg typ -> Map.insert arg (VarN typ)) qs flexiVars
                 env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }
             typ <- instantiateType tipe
             let scheme = Scheme { rigidQuantifiers = [],
                                   flexibleQuantifiers = flexiVars,
                                   constraint = CTrue,
                                   header = Map.singleton name typ }
             c <- constrain env' expr typ
             return ( scheme : schemes
                    , rigidQuantifiers
                    , flexibleQuantifiers
                    , headers
                    , c2
                    , fl rigidQuantifiers c /\ c1 )

      (PVar name, Nothing) ->
          do var <- flexibleVar
             rigidVars <- mapM (\_ -> rigidVar) qs
             let tipe = VarN var
                 inserts = zipWith (\arg typ -> Map.insert arg (VarN typ)) qs rigidVars
                 env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }
             c <- constrain env' expr tipe
             return ( schemes
                    , rigidVars ++ rigidQuantifiers
                    , var : flexibleQuantifiers
                    , Map.insert name tipe headers
                    , c /\ c2
                    , c1 )

instantiateType :: SrcT.Type -> IO Type
instantiateType sourceType = evalStateT (go sourceType) Map.empty
  where
    go :: SrcT.Type -> StateT (Map.Map String Variable) IO Type
    go sourceType =
      case sourceType of
        SrcT.Lambda t1 t2 -> TermN <$> (Fun1 <$> go t1 <*> go t2)

        SrcT.Var x -> do
          dict <- get
          case Map.lookup x dict of
            Just var -> return (VarN var)
            Nothing -> do
              var <- liftIO $ namedVar x -- should this be Constant or Flexible?
              put (Map.insert x var dict)
              return (VarN var)

        SrcT.Data name ts -> do
          ts' <- mapM go ts
          return $ foldr (\t result -> TermN $ App1 t result) (error "not sure how to look this up yet") ts'

        SrcT.EmptyRecord -> return (TermN EmptyRecord1)

        SrcT.Record fields ext ->
          TermN <$> (Record1 <$> traverse (mapM go) fields <*> go ext)

collapseDefs :: [Def t v] -> [(Pattern, LExpr t v, Maybe SrcT.Type)]
collapseDefs = go [] Map.empty Map.empty
  where
    go output defs typs [] =
        output ++ concatMap Map.elems [
          Map.intersectionWithKey (\k v t -> (PVar k, v, Just t)) defs typs,
          Map.mapWithKey (\k v -> (PVar k, v, Nothing)) (Map.difference defs typs) ]
    go output defs typs (d:ds) =
        case d of
          Def (PVar name) body ->
              go output (Map.insert name body defs) typs ds
          Def pattern body ->
              go ((pattern, body, Nothing) : output) defs typs ds
          TypeAnnotation name typ ->
              go output defs (Map.insert name typ typs) ds
--}