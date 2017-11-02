{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Effects
  ( constrain
  )
  where


import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified Elm.Name as N
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Constraint (Constraint(..), Scheme(Scheme))
import Type.Type (Type(..), Variable, (==>), fromFlexSrcType, mkFlexVar)



-- CONSTRAIN


constrain :: ModuleName.Canonical -> Can.Effects -> IO ( Map.Map N.Name Type, Constraint )
constrain home effects =
  case effects of
    Can.NoEffects ->
      return (Map.empty, CSaveEnv)

    Can.Ports ports ->
      (,)
        <$> traverse fromSrcType (Map.map deport ports)
        <*> return CSaveEnv

    Can.Manager r0 r1 r2 manager ->
      case manager of
        Can.Cmd cmdName ->
          do  cmd <- command home cmdName
              con <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home cmdName msg ==> result
              return (Map.singleton "command" cmd, con)

        Can.Sub subName ->
          do  sub <- subscription home subName
              con <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home subName msg ==> result
              return (Map.singleton "subscription" sub, con)

        Can.Fx cmdName subName ->
          do  cmd <- command home cmdName
              sub <- subscription home subName
              con <- constrainEffects r0 r1 r2 $ \msg result ->
                effectList home cmdName msg ==> effectList home subName msg ==> result
              return (Map.fromList [("command", cmd),("subscription", sub)], con)



-- HELPERS


deport :: Can.Port -> T.Canonical
deport port_ =
  case port_ of
    Can.Incoming tipe ->
      tipe

    Can.Outgoing tipe ->
      tipe


fromSrcType :: T.Canonical -> IO Type
fromSrcType tipe =
  fst <$> fromFlexSrcType tipe


command :: ModuleName.Canonical -> N.Name -> IO Type
command home typeName =
  fromSrcType $
    T.Lambda
      (T.Type home typeName [T.Var "msg"])
      (T.Type ModuleName.cmd N.cmd [T.Var "msg"])


subscription :: ModuleName.Canonical -> N.Name -> IO Type
subscription home typeName =
  fromSrcType $
    T.Lambda
      (T.Type home typeName [T.Var "msg"])
      (T.Type ModuleName.sub N.sub [T.Var "msg"])



-- CONSTRAINTS FOR EFFECT MANAGER STUFF


constrainEffects :: R.Region -> R.Region -> R.Region -> (Variable -> Type -> Type) -> IO Constraint
constrainEffects r0 r1 r2 toFinalArgs =
  let
    task t =
      AppN ModuleName.platform N.task [ never, VarN t ]

    router msg selfMsg =
      AppN ModuleName.platform N.router [ VarN msg, VarN selfMsg ]
  in
  do  vars@[v0,v1,v2,s0,s1,s2,msg1,msg2,selfMsg1,selfMsg2] <- replicateM 10 mkFlexVar

      let
        onEffectsType = router msg1 selfMsg1 ==> toFinalArgs msg1 (VarN s1 ==> task s1)
        onSelfMsgType = router msg2 selfMsg2 ==> VarN selfMsg2 ==> VarN s2 ==> task s2
        constrains =
          [ CInstance r0 "init" (VarN v0)
          , CInstance r1 "onEffects" (VarN v1)
          , CInstance r2 "onSelfMsg" (VarN v2)
          , CEqual (Error.Manager "init") r0 (VarN v0) (task s0)
          , CEqual (Error.Manager "onEffects") r1 (VarN v1) onEffectsType
          , CEqual (Error.Manager "onSelfMsg") r2 (VarN v2) onSelfMsgType
          , CEqual (Error.State "onEffects") r1 (VarN s0) (VarN s1)
          , CEqual (Error.State "onSelfMsg") r2 (VarN s0) (VarN s2)
          , CEqual Error.SelfMsg r2 (VarN selfMsg1) (VarN selfMsg2)
          ]

      return $ CLet [ Scheme [] vars (CAnd constrains) Map.empty ] CSaveEnv


{-# NOINLINE never #-}
never :: Type
never =
  AppN ModuleName.basics "Never" []


effectList :: ModuleName.Canonical -> N.Name -> Variable -> Type
effectList home name msg =
  AppN ModuleName.list N.list [AppN home name [VarN msg]]
