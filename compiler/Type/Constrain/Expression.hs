module Type.Constrain.Expression where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad as Monad
import Data.Map ((!))

import SourceSyntax.Location as Loc
import SourceSyntax.Pattern (Pattern(PVar))
import SourceSyntax.Expression
import Type.Type hiding (Descriptor(..))
import Type.Constrain.Fragment
import Type.Constrain.Environment as Env
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern


constrain :: Environment -> LExpr a b -> Type -> IO TypeConstraint
constrain env (L _ _ expr) tipe =
    let list t = TermN (App1 (Env.get env builtin "[]") t) in
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
            return $ CAnd [ c1, c2, (Env.get env value op) === (t1 ==> t2 ==> tipe) ]

      Lambda p e ->
          exists $ \t1 ->
          exists $ \t2 -> do
            fragment <- Pattern.constrain env p t1
            c2 <- constrain env e t2
            let c = ex (vars fragment) (CLet [Scheme [] [] CTrue (typeEnv fragment)]
                                             (typeConstraint fragment /\ c2 ))
            return $ c /\ tipe === (t1 ==> t2)

      App e1 e2 ->
          exists $ \t -> do
            c1 <- constrain env e1 (t ==> tipe)
            c2 <- constrain env e2 t
            return $ c1 /\ c2

      MultiIf branches -> CAnd <$> mapM constrain' branches
          where 
             bool = Env.get env builtin "Bool"
             constrain' (b,e) = do
                  cb <- constrain env b bool
                  ce <- constrain env e tipe
                  return (cb /\ ce)

      Let defs body -> error "not defined yet"

      Case e branches ->
          exists $ \t -> do
            ce <- constrain env e t
            let branch (p,e) = do
                  fragment <- Pattern.constrain env p t
                  c <- constrain env e tipe
                  return $ ex (vars fragment)
                              (CLet [Scheme [] [] CTrue (typeEnv fragment)]
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
            field (name, args, body) = do
                let value = List.foldl' (\e arg -> Loc.none (Lambda (PVar arg) e)) body args
                v <- flexibleVar -- needs an ex
                c <- constrain env value (VarN v)
                return ((name, v), c)
                

      Markdown _ ->
          return $ tipe === Env.get env builtin "Element"


--collapseDefs :: [Def t v] -> [(String, [String], LExpr t v, Maybe Type)]
collapseDefs = go Map.empty Map.empty
    where
      go defs typs [] = Map.union (Map.intersectionWith (\f t -> f (Just t)) defs typs)
                                  (Map.map ($ Nothing) (Map.difference defs typs))
      go defs typs (d:ds) =
          case d of
            Def name args body ->
                go (Map.insert name ((,,) args body) defs) typs ds
            TypeAnnotation name typ ->
                go defs (Map.insert name typ typs) ds
          