{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Effects (constrain) where

import qualified Data.Map.Strict as Map

import qualified AST.Effects as Effects
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import Type.Constraint (Constraint(..), Scheme(Scheme))
import Type.Type (Variable, Type(AppN,VarN), (==>), mkFlexVar)



-- CONSTRAIN EFFECTS


constrain :: ModuleName.Canonical -> Effects.Canonical -> IO Constraint
constrain moduleName effects =
  case effects of
    Effects.None ->
      return CSaveEnv

    Effects.Port _ ->
      return CSaveEnv

    Effects.Manager _ info ->
      constrainHelp moduleName info


never :: Type
never =
  AppN Var.never []


constrainHelp :: ModuleName.Canonical -> Effects.Info -> IO Constraint
constrainHelp moduleName (Effects.Info tagRegion r0 r1 r2 managerType) =
  let
    task t =
      AppN Var.task [ never, VarN t ]

    router msg selfMsg =
      AppN Var.router [ VarN msg, VarN selfMsg ]
  in
    do  v0 <- mkFlexVar
        v1 <- mkFlexVar
        v2 <- mkFlexVar

        state0 <- mkFlexVar
        state1 <- mkFlexVar
        state2 <- mkFlexVar

        msg1 <- mkFlexVar
        msg2 <- mkFlexVar

        selfMsg1 <- mkFlexVar
        selfMsg2 <- mkFlexVar

        let
          vars =
            [v0, v1, v2, state0, state1, state2, msg1, msg2, selfMsg1, selfMsg2]

          onEffectsType =
            router msg1 selfMsg1
            ==> addEffectArgs moduleName managerType msg1
                  (VarN state1 ==> task state1)

          onSelfMsgType =
            router msg2 selfMsg2 ==> VarN selfMsg2 ==> VarN state2 ==> task state2

          constrains =
            [ CInstance tagRegion "init" (VarN v0)
            , CInstance tagRegion "onEffects" (VarN v1)
            , CInstance tagRegion "onSelfMsg" (VarN v2)
            , CEqual (Error.Manager "init") r0 (VarN v0) (task state0)
            , CEqual (Error.Manager "onEffects") r1 (VarN v1) onEffectsType
            , CEqual (Error.Manager "onSelfMsg") r2 (VarN v2) onSelfMsgType
            , CEqual (Error.State "onEffects") r1 (VarN state0) (VarN state1)
            , CEqual (Error.State "onSelfMsg") r2 (VarN state0) (VarN state2)
            , CEqual Error.SelfMsg r2 (VarN selfMsg1) (VarN selfMsg2)
            ]

        return $
          CLet [Scheme [] vars (CAnd constrains) Map.empty] CSaveEnv


addEffectArgs
  :: ModuleName.Canonical
  -> Effects.RawManagerType
  -> Variable
  -> Type
  -> Type
addEffectArgs moduleName managerType msg result =
  let
    toVar (A.A _ name) =
      Var.fromModule moduleName name

    effectList fxName =
      AppN Var.list [AppN (toVar fxName) [VarN msg]]
  in
    case managerType of
      Effects.CmdManager cmd ->
        effectList cmd ==> result

      Effects.SubManager sub ->
        effectList sub ==> result

      Effects.FxManager cmd sub ->
        effectList cmd ==> effectList sub ==> result

