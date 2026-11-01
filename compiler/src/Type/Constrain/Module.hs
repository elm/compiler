{-# LANGUAGE DataKinds, OverloadedStrings, QuasiQuotes #-}
module Type.Constrain.Module
  ( constrain
  )
  where


import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map

import qualified Array as A
import qualified Graph

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Type.Constrain.Expression as Expr
import Type.Type (Type(..), Constraint(..), (==>), mkFlexAny, varToRigid, never)



-- CONSTRAIN


constrain :: Can.Module -> IO Constraint
constrain (Can.Module home _ _ decls _ _ style) =
  case style of
    Can.Normal ->
      constrainDecls decls CSaveTheEnvironment

    Can.Ports ports ->
      Map.foldrWithKey letPort (constrainDecls decls CSaveTheEnvironment) ports

    Can.Effect r0 r1 r2 manager ->
      case manager of
        Can.Cmd cmdName ->
          letCmd home cmdName =<<
            constrainDecls decls =<< conEffects home r0 r1 r2 manager

        Can.Sub subName ->
          letSub home subName =<<
            constrainDecls decls =<< conEffects home r0 r1 r2 manager

        Can.Fx cmdName subName ->
          letCmd home cmdName =<<
          letSub home subName =<<
            constrainDecls decls =<< conEffects home r0 r1 r2 manager



-- CONSTRAIN DECLARATIONS


constrainDecls :: Can.Decls -> Constraint -> IO Constraint
constrainDecls (Can.Decls (Graph.Sorted decls)) finalConstraint =
  foldrM Expr.constrainDecl finalConstraint decls



-- PORT HELPERS


letPort :: N.Name -> Can.Port -> IO Constraint -> IO Constraint
letPort name port_ makeConstraint =
  case port_ of
    Can.Incoming freeVars _ tipe ->
      do  vars <- A.traverseIO varToRigid freeVars
          let header = Map.singleton name (A.At A.zero (TypeN vars tipe))
          CLet (A.toList vars) [] header CTrue <$> makeConstraint

    Can.Outgoing freeVars _ tipe ->
      do  vars <- A.traverseIO varToRigid freeVars
          let header = Map.singleton name (A.At A.zero (TypeN vars tipe))
          CLet (A.toList vars) [] header CTrue <$> makeConstraint



-- EFFECT MANAGER HELPERS


letCmd :: ModuleName.Canonical -> Can.EType Can.CMD -> Constraint -> IO Constraint
letCmd home tipe constraint =
  do  msgVar <- mkFlexAny
      let msg = VarN msgVar
      let cmdType = FunN (eTypeT home tipe msg) (cmdT msg)
      let header = Map.singleton [N.ascii|command|] (A.At A.zero cmdType)
      return $ CLet [msgVar] [] header CTrue constraint


letSub :: ModuleName.Canonical -> Can.EType Can.SUB -> Constraint -> IO Constraint
letSub home tipe constraint =
  do  msgVar <- mkFlexAny
      let msg = VarN msgVar
      let subType = FunN (eTypeT home tipe msg) (subT msg)
      let header = Map.singleton [N.ascii|subscription|] (A.At A.zero subType)
      return $ CLet [msgVar] [] header CTrue constraint


conEffects :: ModuleName.Canonical -> A.Region -> A.Region -> A.Region -> Can.Manager -> IO Constraint
conEffects home r0 r1 r2 manager =
  do  s0 <- mkFlexAny
      s1 <- mkFlexAny
      s2 <- mkFlexAny
      m1 <- mkFlexAny
      m2 <- mkFlexAny
      sm1 <- mkFlexAny
      sm2 <- mkFlexAny

      let state0 = VarN s0
      let state1 = VarN s1
      let state2 = VarN s2
      let msg1 = VarN m1
      let msg2 = VarN m2
      let self1 = VarN sm1
      let self2 = VarN sm2

      let onSelfMsg = routerT msg2 self2 ==> self2 ==> state2 ==> taskT state2
      let onEffects =
            case manager of
              Can.Cmd cmd    -> routerT msg1 self1 ==> listT (eTypeT home cmd msg1) ==> state1 ==> taskT state1
              Can.Sub sub    -> routerT msg1 self1 ==> listT (eTypeT home sub msg1) ==> state1 ==> taskT state1
              Can.Fx cmd sub -> routerT msg1 self1 ==> listT (eTypeT home cmd msg1) ==> listT (eTypeT home sub msg1) ==> state1 ==> taskT state1

      let effectCons =
            CAnd
              [ CLocal r0 [N.ascii|init|] (E.NoExpectation (taskT state0))
              , CLocal r1 [N.ascii|onEffects|] (E.NoExpectation onEffects)
              , CLocal r2 [N.ascii|onSelfMsg|] (E.NoExpectation onSelfMsg)
              , CEqual r1 E.Effects state0 (E.NoExpectation state1)
              , CEqual r2 E.Effects state0 (E.NoExpectation state2)
              , CEqual r2 E.Effects self1 (E.NoExpectation self2)
              ]

      CLet [] [s0,s1,s2,m1,m2,sm1,sm2] Map.empty effectCons <$>
        case manager of
          Can.Cmd cmd ->
            checkMap [N.ascii|cmdMap|] home cmd CSaveTheEnvironment

          Can.Sub sub ->
            checkMap [N.ascii|subMap|] home sub CSaveTheEnvironment

          Can.Fx cmd sub ->
            checkMap [N.ascii|cmdMap|] home cmd =<<
            checkMap [N.ascii|subMap|] home sub CSaveTheEnvironment


checkMap :: N.Name -> ModuleName.Canonical -> Can.EType k -> Constraint -> IO Constraint
checkMap name home tipe constraint =
  do  a <- mkFlexAny
      b <- mkFlexAny
      let mapType = eTypeMapT home tipe (VarN a) (VarN b)
      let mapCon = CLocal A.zero name (E.NoExpectation mapType)
      return $ CLet [a,b] [] Map.empty mapCon constraint



-- HELPERS


listT     :: Type -> Type
taskT     :: Type -> Type
routerT   :: Type -> Type -> Type
cmdT      :: Type -> Type
subT      :: Type -> Type

listT a           = AppN ModuleName.list T.list M.list (A.from1 a)
taskT answer      = AppN ModuleName.platform T.task M.task (A.from2 never answer)
routerT msg self  = AppN ModuleName.platform T.router M.router (A.from2 msg self)
cmdT msg          = AppN ModuleName.platform_cmd T.cmd M.cmd (A.from1 msg)
subT msg          = AppN ModuleName.platform_sub T.sub M.sub (A.from1 msg)


eTypeT :: ModuleName.Canonical -> Can.EType k -> Type -> Type
eTypeT h (Can.EType n m) msg =
  AppN h n m (A.from1 msg)


eTypeMapT :: ModuleName.Canonical -> Can.EType k -> Type -> Type -> Type
eTypeMapT h t a b =
  (a ==> b) ==> eTypeT h t a ==> eTypeT h t b


