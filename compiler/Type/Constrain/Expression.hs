{-# OPTIONS_GHC -W #-}
module Type.Constrain.Expression where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import Control.Monad.Error
import qualified Text.PrettyPrint as PP

import SourceSyntax.Location as Loc
import SourceSyntax.Pattern (Pattern(PVar), boundVars)
import SourceSyntax.Expression
import qualified SourceSyntax.Type as SrcT
import Type.Type hiding (Descriptor(..))
import Type.Fragment
import qualified Type.Environment as Env
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern

constrain :: Env.Environment -> LExpr -> Type -> ErrorT [PP.Doc] IO TypeConstraint
constrain env (L span expr) tipe =
    let list t = Env.get env Env.types "_List" <| t
        and = L span . CAnd
        true = L span CTrue
        t1 === t2 = L span (CEqual t1 t2)
        x <? t = L span (CInstance x t)
        clet schemes c = L span (CLet schemes c)
    in
    case expr of
      Literal lit -> liftIO $ Literal.constrain env span lit tipe

      Var name | name == saveEnvName -> return (L span CSaveEnv)
               | otherwise           -> return (name <? tipe)

      Range lo hi ->
          exists $ \x -> do
            clo <- constrain env lo x
            chi <- constrain env hi x
            return $ and [clo, chi, list x === tipe]

      ExplicitList exprs ->
          exists $ \x -> do
            cs <- mapM (\e -> constrain env e x) exprs
            return . and $ list x === tipe : cs

      Binop op e1 e2 ->
          exists $ \t1 ->
          exists $ \t2 -> do
            c1 <- constrain env e1 t1
            c2 <- constrain env e2 t2
            return $ and [ c1, c2, op <? (t1 ==> t2 ==> tipe) ]

      Lambda p e ->
          exists $ \t1 ->
          exists $ \t2 -> do
            fragment <- try span $ Pattern.constrain env p t1
            c2 <- constrain env e t2
            let c = ex (vars fragment) (clet [monoscheme (typeEnv fragment)]
                                             (typeConstraint fragment /\ c2 ))
            return $ c /\ tipe === (t1 ==> t2)

      App e1 e2 ->
          exists $ \t -> do
            c1 <- constrain env e1 (t ==> tipe)
            c2 <- constrain env e2 t
            return $ c1 /\ c2

      MultiIf branches -> and <$> mapM constrain' branches
          where 
             bool = Env.get env Env.types "Bool"
             constrain' (b,e) = do
                  cb <- constrain env b bool
                  ce <- constrain env e tipe
                  return (cb /\ ce)

      Case exp branches ->
          exists $ \t -> do
            ce <- constrain env exp t
            let branch (p,e) = do
                  fragment <- try span $ Pattern.constrain env p t
                  clet [toScheme fragment] <$> constrain env e tipe
            and . (:) ce <$> mapM branch branches

      Data name exprs ->
          do vars <- forM exprs $ \_ -> liftIO (var Flexible)
             let pairs = zip exprs (map VarN vars)
             (ctipe, cs) <- Monad.foldM step (tipe,true) (reverse pairs)
             return $ ex vars (cs /\ name <? ctipe)
          where
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
              return (and [cVal, cRec, c])

      Modify e fields ->
          exists $ \t -> do
              oldVars <- forM fields $ \_ -> liftIO (var Flexible)
              let oldFields = SrcT.fieldMap (zip (map fst fields) (map VarN oldVars))
              cOld <- ex oldVars <$> constrain env e (record oldFields t)

              newVars <- forM fields $ \_ -> liftIO (var Flexible)
              let newFields = SrcT.fieldMap (zip (map fst fields) (map VarN newVars))
              let cNew = tipe === record newFields t

              cs <- zipWithM (constrain env) (map snd fields) (map VarN newVars)

              return $ cOld /\ ex newVars (and (cNew : cs))

      Record fields ->
          do vars <- forM fields $ \_ -> liftIO (var Flexible)
             cs <- zipWithM (constrain env) (map snd fields) (map VarN vars)
             let fields' = SrcT.fieldMap (zip (map fst fields) (map VarN vars))
                 recordType = record fields' (TermN EmptyRecord1)
             return . ex vars . and $ tipe === recordType : cs

      Markdown _ _ es ->
          do vars <- forM es $ \_ -> liftIO (var Flexible)
             let tvars = map VarN vars
             cs <- zipWithM (constrain env) es tvars
             return . ex vars $ and ("Text.markdown" <? tipe : cs)

      Let defs body ->
          do c <- constrain env body tipe
             (schemes, rqs, fqs, header, c2, c1) <-
                 Monad.foldM (constrainDef env)
                             ([], [], [], Map.empty, true, true)
                             (concatMap expandPattern defs)
             return $ clet schemes
                           (clet [Scheme rqs fqs (clet [monoscheme header] c2) header ]
                                 (c1 /\ c))

      PortIn _ _ -> return true

      PortOut _ _ signal ->
          constrain env signal tipe

constrainDef env info (Definition pattern expr maybeTipe) =
    let qs = [] -- should come from the def, but I'm not sure what would live there...
        (schemes, rigidQuantifiers, flexibleQuantifiers, headers, c2, c1) = info
    in
    do rigidVars <- forM qs (\_ -> liftIO $ var Rigid) -- Some mistake may be happening here.
                                                       -- Currently, qs is always [].
       case (pattern, maybeTipe) of
         (PVar name, Just tipe) -> do
             flexiVars <- forM qs (\_ -> liftIO $ var Flexible)
             let inserts = zipWith (\arg typ -> Map.insert arg (VarN typ)) qs flexiVars
                 env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }
             (vars, typ) <- Env.instantiateType env tipe Map.empty
             let scheme = Scheme { rigidQuantifiers = [],
                                   flexibleQuantifiers = flexiVars ++ vars,
                                   constraint = Loc.noneNoDocs CTrue,
                                   header = Map.singleton name typ }
             c <- constrain env' expr typ
             return ( scheme : schemes
                    , rigidQuantifiers
                    , flexibleQuantifiers
                    , headers
                    , c2
                    , fl rigidVars c /\ c1 )

         (PVar name, Nothing) -> do
             v <- liftIO $ var Flexible
             let tipe = VarN v
                 inserts = zipWith (\arg typ -> Map.insert arg (VarN typ)) qs rigidVars
                 env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }
             c <- constrain env' expr tipe
             return ( schemes
                    , rigidVars ++ rigidQuantifiers
                    , v : flexibleQuantifiers
                    , Map.insert name tipe headers
                    , c /\ c2
                    , c1 )

         _ -> error (show pattern)

expandPattern :: Def -> [Def]
expandPattern def@(Definition pattern lexpr@(L s _) maybeType) =
    case pattern of
      PVar _ -> [def]
      _ -> Definition (PVar x) lexpr maybeType : map toDef vars
          where
            vars = Set.toList $ boundVars pattern
            x = "$" ++ concat vars
            mkVar = L s . Var
            toDef y = Definition (PVar y) (L s $ Case (mkVar x) [(pattern, mkVar y)]) Nothing

try :: SrcSpan -> ErrorT (SrcSpan -> PP.Doc) IO a -> ErrorT [PP.Doc] IO a
try span computation = do
  result <- liftIO $ runErrorT computation
  case result of
    Left err -> throwError [err span]
    Right value -> return value
