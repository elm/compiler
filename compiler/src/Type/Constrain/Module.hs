{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Type.Constrain.Module
  ( constrain
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Type.Constrain.Expression as Expr
import qualified Type.Instantiate as Instantiate
import Type.Type (Type(..), Constraint(..), (==>), mkFlexVar, nameToRigid, never)



-- CONSTRAIN


constrain :: Can.Module -> IO Constraint
constrain (Can.Module home _ _ decls _ _ _ effects) =
  case effects of
    Can.NoEffects ->
      constrainDecls decls CSaveTheEnvironment

    Can.Ports ports ->
      Map.foldrWithKey letPort (constrainDecls decls CSaveTheEnvironment) ports

    Can.Manager r0 r1 r2 manager ->
      case manager of
        Can.Cmd cmdName ->
          letCmd home cmdName =<<
            constrainDecls decls =<< constrainEffects home r0 r1 r2 manager

        Can.Sub subName ->
          letSub home subName =<<
            constrainDecls decls =<< constrainEffects home r0 r1 r2 manager

        Can.Fx cmdName subName ->
          letCmd home cmdName =<<
          letSub home subName =<<
            constrainDecls decls =<< constrainEffects home r0 r1 r2 manager



-- CONSTRAIN DECLARATIONS


constrainDecls :: Can.Decls -> Constraint -> IO Constraint
constrainDecls decls finalConstraint =
  case decls of
    Can.Declare def otherDecls ->
      Expr.constrainDef Map.empty def =<< constrainDecls otherDecls finalConstraint

    Can.DeclareRec def defs otherDecls ->
      Expr.constrainRecursiveDefs Map.empty (def:defs) =<< constrainDecls otherDecls finalConstraint

    Can.SaveTheEnvironment ->
      return finalConstraint



-- PORT HELPERS


letPort :: N.Name -> Can.Port -> IO Constraint -> IO Constraint
letPort name port_ makeConstraint =
  case port_ of
    Can.Incoming freeVars _ srcType ->
      do  vars <- Map.traverseWithKey (\k _ -> nameToRigid k) freeVars
          tipe <- Instantiate.fromSrcType (Map.map VarN vars) srcType
          let header = Map.singleton name (A.At A.zero tipe)
          CLet (Map.elems vars) [] header CTrue <$> makeConstraint

    Can.Outgoing freeVars _ srcType ->
      do  vars <- Map.traverseWithKey (\k _ -> nameToRigid k) freeVars
          tipe <- Instantiate.fromSrcType (Map.map VarN vars) srcType
          let header = Map.singleton name (A.At A.zero tipe)
          CLet (Map.elems vars) [] header CTrue <$> makeConstraint



-- EFFECT MANAGER HELPERS


letCmd :: ModuleName.Canonical -> T.Name -> Constraint -> IO Constraint
letCmd home tipe constraint =
  do  msgVar <- mkFlexVar
      let msg = VarN msgVar
      let cmdType = FunN (AppN home tipe [msg]) (AppN ModuleName.platform_cmd T.cmd [msg])
      let header = Map.singleton [N.ascii|command|] (A.At A.zero cmdType)
      return $ CLet [msgVar] [] header CTrue constraint


letSub :: ModuleName.Canonical -> T.Name -> Constraint -> IO Constraint
letSub home tipe constraint =
  do  msgVar <- mkFlexVar
      let msg = VarN msgVar
      let subType = FunN (AppN home tipe [msg]) (AppN ModuleName.platform_sub T.sub [msg])
      let header = Map.singleton [N.ascii|subscription|] (A.At A.zero subType)
      return $ CLet [msgVar] [] header CTrue constraint


constrainEffects :: ModuleName.Canonical -> A.Region -> A.Region -> A.Region -> Can.Manager -> IO Constraint
constrainEffects home r0 r1 r2 manager =
  do  s0 <- mkFlexVar
      s1 <- mkFlexVar
      s2 <- mkFlexVar
      m1 <- mkFlexVar
      m2 <- mkFlexVar
      sm1 <- mkFlexVar
      sm2 <- mkFlexVar

      let state0 = VarN s0
      let state1 = VarN s1
      let state2 = VarN s2
      let msg1 = VarN m1
      let msg2 = VarN m2
      let self1 = VarN sm1
      let self2 = VarN sm2

      let onSelfMsg = router msg2 self2 ==> self2 ==> state2 ==> task state2
      let onEffects =
            case manager of
              Can.Cmd cmd    -> router msg1 self1 ==> effectList home cmd msg1 ==> state1 ==> task state1
              Can.Sub sub    -> router msg1 self1 ==> effectList home sub msg1 ==> state1 ==> task state1
              Can.Fx cmd sub -> router msg1 self1 ==> effectList home cmd msg1 ==> effectList home sub msg1 ==> state1 ==> task state1

      let effectCons =
            CAnd
              [ CLocal r0 [N.ascii|init|] (E.NoExpectation (task state0))
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


effectList :: ModuleName.Canonical -> T.Name -> Type -> Type
effectList home name msg =
  AppN ModuleName.list T.list [AppN home name [msg]]


task :: Type -> Type
task answer =
  AppN ModuleName.platform T.task [ never, answer ]


router :: Type -> Type -> Type
router msg self =
  AppN ModuleName.platform T.router [ msg, self ]


checkMap :: N.Name -> ModuleName.Canonical -> T.Name -> Constraint -> IO Constraint
checkMap name home tipe constraint =
  do  a <- mkFlexVar
      b <- mkFlexVar
      let mapType = toMapType home tipe (VarN a) (VarN b)
      let mapCon = CLocal A.zero name (E.NoExpectation mapType)
      return $ CLet [a,b] [] Map.empty mapCon constraint


toMapType :: ModuleName.Canonical -> T.Name -> Type -> Type -> Type
toMapType home tipe a b =
  (a ==> b) ==> AppN home tipe [a] ==> AppN home tipe [b]
