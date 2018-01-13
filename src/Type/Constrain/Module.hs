{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Module
  ( constrain
  )
  where


import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Constrain.Expression as Expr
import qualified Type.Instantiate as Instantiate
import Type.Type (Type(..), Variable, (==>), mkFlexVar, never)



-- CONSTRAIN


constrain :: Can.Module -> IO Constraint
constrain (Can.Module home _ _ decls _ _ _ effects) =
  case effects of
    Can.NoEffects ->
      constrainDecls CSaveTheEnvironment decls

    Can.Ports ports ->
      do  header <- traverse portToHeader ports
          CLet [] [] header CTrue
            <$> constrainDecls CSaveTheEnvironment decls

    Can.Manager r0 r1 r2 manager ->
      case manager of
        Can.Cmd cmdName ->
          do  header <- mkCmd home cmdName
              fxCon <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home cmdName msg ==> result
              CLet [] [] header CTrue <$> constrainDecls fxCon decls

        Can.Sub subName ->
          do  header <- mkSub home subName
              fxCon <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home subName msg ==> result
              CLet [] [] header CTrue <$> constrainDecls fxCon decls

        Can.Fx cmdName subName ->
          do  header <- Map.union <$> mkCmd home cmdName <*> mkSub home subName
              fxCon <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home cmdName msg ==> effectList home subName msg ==> result
              CLet [] [] header CTrue <$> constrainDecls fxCon decls



-- CONSTRAIN DECLARATIONS


constrainDecls :: Constraint -> Can.Decls -> IO Constraint
constrainDecls finalConstraint decls =
  case decls of
    Can.Declare def otherDecls ->
      Expr.constrainDef Map.empty def
        =<< constrainDecls finalConstraint otherDecls

    Can.DeclareRec defs otherDecls ->
      Expr.constrainRecursiveDefs Map.empty defs
        =<< constrainDecls finalConstraint otherDecls

    Can.SaveTheEnvironment ->
      return finalConstraint



-- PORT HELPERS


portToHeader :: Can.Port -> IO (A.Located Type)
portToHeader port_ =
  case port_ of
    Can.Incoming freeVars tipe ->
      do  vars <- traverse (\_ -> VarN <$> mkFlexVar) freeVars
          A.At zero <$> Instantiate.fromSrcType vars tipe

    Can.Outgoing freeVars tipe ->
      do  vars <- traverse (\_ -> VarN <$> mkFlexVar) freeVars
          A.At zero <$> Instantiate.fromSrcType vars tipe


{-# NOINLINE zero #-}
zero :: R.Region
zero =
  R.Region (R.Position 0 0) (R.Position 0 0)



-- EFFECT MANAGER HELPERS


mkCmd :: ModuleName.Canonical -> N.Name -> IO (Map.Map N.Name (A.Located Type))
mkCmd home typeName =
  do  msg <- VarN <$> mkFlexVar
      return $ Map.singleton "command" $ A.At zero $
        FunN (AppN home typeName [msg]) (AppN ModuleName.cmd N.cmd [msg])


mkSub :: ModuleName.Canonical -> N.Name -> IO (Map.Map N.Name (A.Located Type))
mkSub home typeName =
  do  msg <- VarN <$> mkFlexVar
      return $ Map.singleton "subscription" $ A.At zero $
        FunN (AppN home typeName [msg]) (AppN ModuleName.sub N.sub [msg])


constrainEffects
  :: R.Region
  -> R.Region
  -> R.Region
  -> (Variable -> Type -> Type)
  -> IO Constraint
constrainEffects r0 r1 r2 toFinalArgs =
  let
    task t =
      AppN ModuleName.platform N.task [ never, VarN t ]

    router msg selfMsg =
      AppN ModuleName.platform N.router [ VarN msg, VarN selfMsg ]

    equal region var tipe =
      CEqual region Effects (VarN var) (NoExpectation tipe)
  in
  do  vars@[s0,s1,s2,msg1,msg2,selfMsg1,selfMsg2] <- replicateM 7 mkFlexVar
      return $
        CLet [] vars Map.empty
          (CAnd
            [ CLocal r0 "init" $ NoExpectation $
                task s0
            , CLocal r1 "onEffects" $ NoExpectation $
                router msg1 selfMsg1 ==> toFinalArgs msg1 (VarN s1 ==> task s1)
            , CLocal r2 "onSelfMsg" $ NoExpectation $
                router msg2 selfMsg2 ==> VarN selfMsg2 ==> VarN s2 ==> task s2
            , equal r1 s0 (VarN s1)
            , equal r2 s0 (VarN s2)
            , equal r2 selfMsg1 (VarN selfMsg2)
            ]
          )
          CSaveTheEnvironment


effectList :: ModuleName.Canonical -> N.Name -> Variable -> Type
effectList home name msg =
  AppN ModuleName.list N.list [AppN home name [VarN msg]]
