{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Expression where

import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import Control.Monad.Error
import qualified Data.List as List
import qualified Data.Map as Map
import Prelude hiding (and)
import qualified Text.PrettyPrint as PP

import qualified AST.Literal as Lit
import AST.Annotation as Ann
import AST.Expression.General
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified AST.Type as ST
import qualified AST.Variable as V
import Type.Type hiding (Descriptor(..))
import qualified Type.Fragment as Fragment
import qualified Type.Environment as Env
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern


constrain
    :: Env.Environment
    -> Canonical.Expr
    -> Type
    -> ErrorT [PP.Doc] IO TypeConstraint
constrain env (A region expression) tipe =
    let list t = Env.get env Env.types "List" <| t
        and = A region . CAnd
        true = A region CTrue
        t1 === t2 = A region (CEqual t1 t2)
        x <? t = A region (CInstance x t)
        clet schemes c = A region (CLet schemes c)
    in
    case expression of
      Literal lit ->
          liftIO $ Literal.constrain env region lit tipe

      GLShader _uid _src gltipe ->
          exists $ \attr ->
          exists $ \unif ->
            let
              shaderTipe a u v = Env.get env Env.types "WebGL.Shader" <| a <| u <| v
              glTipe = Env.get env Env.types . Lit.glTipeName
              makeRec accessor baseRec =
                let decls = accessor gltipe
                in if Map.size decls == 0
                   then baseRec
                   else record (Map.map (\t -> [glTipe t]) decls) baseRec
              attribute = makeRec Lit.attribute attr
              uniform = makeRec Lit.uniform unif
              varying = makeRec Lit.varying (termN EmptyRecord1)
            in
              return . A region $ CEqual tipe (shaderTipe attribute uniform varying)

      Var var
          | name == saveEnvName -> return (A region CSaveEnv)
          | otherwise           -> return (name <? tipe)
          where
            name = V.toString var

      Range lo hi ->
          existsNumber $ \n ->
              do  clo <- constrain env lo n
                  chi <- constrain env hi n
                  return $ and [clo, chi, list n === tipe]

      ExplicitList exprs ->
          exists $ \x ->
              do  constraints <- mapM (\e -> constrain env e x) exprs
                  return . and $ list x === tipe : constraints

      Binop op e1 e2 ->
          exists $ \t1 ->
          exists $ \t2 ->
              do  c1 <- constrain env e1 t1
                  c2 <- constrain env e2 t2
                  return $ and [ c1, c2, V.toString op <? (t1 ==> t2 ==> tipe) ]

      Lambda p e ->
          exists $ \t1 ->
          exists $ \t2 ->
              do  fragment <- try region $ Pattern.constrain env p t1
                  c2 <- constrain env e t2
                  let c = ex (Fragment.vars fragment)
                             (clet [monoscheme (Fragment.typeEnv fragment)]
                                   (Fragment.typeConstraint fragment /\ c2)
                             )
                  return $ c /\ tipe === (t1 ==> t2)

      App e1 e2 ->
          exists $ \t ->
              do  c1 <- constrain env e1 (t ==> tipe)
                  c2 <- constrain env e2 t
                  return $ c1 /\ c2

      MultiIf branches ->
          and <$> mapM constrain' branches
        where
          bool = Env.get env Env.types "Bool"

          constrain' (cond, expr) =
              do  cb <- constrain env cond bool
                  ce <- constrain env expr tipe
                  return (cb /\ ce)

      Case expr branches ->
          exists $ \t ->
              do  ce <- constrain env expr t
                  and . (:) ce <$> mapM (branch t) branches
          where
            branch t (pattern, branchExpr) =
                do  fragment <- try region $ Pattern.constrain env pattern t
                    clet [Fragment.toScheme fragment] <$> constrain env branchExpr tipe

      Data name exprs ->
          do  vars <- forM exprs $ \_ -> liftIO (variable Flexible)
              let pairs = zip exprs (map varN vars)
              (ctipe, cs) <- Monad.foldM step (tipe,true) (reverse pairs)
              return $ ex vars (cs /\ name <? ctipe)
          where
            step (t,c) (e,x) =
                do  c' <- constrain env e x
                    return (x ==> t, c /\ c')

      Access expr label ->
          exists $ \t ->
              constrain env expr (record (Map.singleton label [tipe]) t)

      Remove expr label ->
          exists $ \t ->
              constrain env expr (record (Map.singleton label [t]) tipe)

      Insert expr label value ->
          exists $ \tVal ->
          exists $ \tRec ->
              do  cVal <- constrain env value tVal
                  cRec <- constrain env expr tRec
                  let c = tipe === record (Map.singleton label [tVal]) tRec
                  return (and [cVal, cRec, c])

      Modify expr fields ->
          exists $ \t ->
              do  oldVars <- forM fields $ \_ -> liftIO (variable Flexible)
                  let oldFields = ST.fieldMap (zip (map fst fields) (map varN oldVars))
                  cOld <- ex oldVars <$> constrain env expr (record oldFields t)

                  newVars <- forM fields $ \_ -> liftIO (variable Flexible)
                  let newFields = ST.fieldMap (zip (map fst fields) (map varN newVars))
                  let cNew = tipe === record newFields t

                  cs <- zipWithM (constrain env) (map snd fields) (map varN newVars)

                  return $ cOld /\ ex newVars (and (cNew : cs))

      Record fields ->
          do  vars <- forM fields $ \_ -> liftIO (variable Flexible)
              cs <- zipWithM (constrain env) (map snd fields) (map varN vars)
              let fields' = ST.fieldMap (zip (map fst fields) (map varN vars))
              let recordType = record fields' (termN EmptyRecord1)
              return . ex vars . and $ tipe === recordType : cs

      Let defs body ->
          do  c <- constrain env body tipe

              (Info schemes rqs fqs headers c2 c1) <-
                  Monad.foldM
                      (constrainDef env)
                      (Info [] [] [] Map.empty true true)
                      (concatMap expandPattern defs)

              let letScheme = [ Scheme rqs fqs (clet [monoscheme headers] c2) headers ]

              return $ clet schemes (clet letScheme (c1 /\ c))

      Port impl ->
          case impl of
            In _ _ ->
                return true

            Out _ expr _ ->
                constrain env expr tipe

            Task _ expr _ ->
                constrain env expr tipe


expandPattern :: Canonical.Def -> [Canonical.Def]
expandPattern def@(Canonical.Definition pattern lexpr@(A r _) maybeType) =
    case pattern of
      P.Var _ ->
          [def]
      _ ->
          Canonical.Definition (P.Var x) lexpr maybeType : map toDef vars
        where
          vars = P.boundVarList pattern
          x = "$" ++ concat vars
          mkVar = A r . localVar
          toDef y = Canonical.Definition (P.Var y) (A r $ Case (mkVar x) [(pattern, mkVar y)]) Nothing


try :: Region -> ErrorT (Region -> PP.Doc) IO a -> ErrorT [PP.Doc] IO a
try region computation =
  do  result <- liftIO $ runErrorT computation
      case result of
        Left err -> throwError [err region]
        Right value -> return value


-- CONSTRAIN DEFINITIONS

data Info = Info
    { iSchemes :: [TypeScheme]
    , iRigid :: [Variable]
    , iFlex :: [Variable]
    , iHeaders :: Map.Map String Type
    , iC2 :: TypeConstraint
    , iC1 :: TypeConstraint
    }


constrainDef :: Env.Environment -> Info -> Canonical.Def -> ErrorT [PP.Doc] IO Info
constrainDef env info (Canonical.Definition pattern expr maybeTipe) =
  let qs = [] -- should come from the def, but I'm not sure what would live there...
  in
  case (pattern, maybeTipe) of
    (P.Var name, Just tipe) ->
        constrainAnnotatedDef env info qs name expr tipe

    (P.Var name, Nothing) ->
        constrainUnannotatedDef env info qs name expr

    _ -> error ("problem in constrainDef with " ++ show pattern)


constrainAnnotatedDef
    :: Env.Environment
    -> Info
    -> [String]
    -> String
    -> Canonical.Expr
    -> ST.CanonicalType
    -> ErrorT [PP.Doc] IO Info
constrainAnnotatedDef env info qs name expr tipe =
  do  -- Some mistake may be happening here. Currently, qs is always [].
      rigidVars <- forM qs (\_ -> liftIO $ variable Rigid)

      flexiVars <- forM qs (\_ -> liftIO $ variable Flexible)

      let inserts = zipWith (\arg typ -> Map.insert arg (varN typ)) qs flexiVars

      let env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }

      (vars, typ) <- Env.instantiateType env tipe Map.empty

      let scheme =
            Scheme
            { rigidQuantifiers = []
            , flexibleQuantifiers = flexiVars ++ vars
            , constraint = Ann.noneNoDocs CTrue
            , header = Map.singleton name typ
            }

      c <- constrain env' expr typ

      return $ info
          { iSchemes = scheme : iSchemes info
          , iC1 = fl rigidVars c /\ iC1 info
          }


constrainUnannotatedDef
    :: Env.Environment
    -> Info
    -> [String]
    -> String
    -> Canonical.Expr
    -> ErrorT [PP.Doc] IO Info
constrainUnannotatedDef env info qs name expr =
  do  -- Some mistake may be happening here. Currently, qs is always [].
      rigidVars <- forM qs (\_ -> liftIO $ variable Rigid)

      v <- liftIO $ variable Flexible

      let tipe = varN v

      let inserts = zipWith (\arg typ -> Map.insert arg (varN typ)) qs rigidVars

      let env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }

      c <- constrain env' expr tipe

      return $ info
          { iRigid = rigidVars ++ iRigid info
          , iFlex = v : iFlex info
          , iHeaders = Map.insert name tipe (iHeaders info)
          , iC2 = c /\ iC2 info
          }
