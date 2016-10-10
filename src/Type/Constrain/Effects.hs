{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Effects (constrain) where

import qualified Data.Map as Map

import qualified AST.Effects as Effects
import qualified AST.Module.Name as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.Environment as Env
import Type.Type
  ( Variable, Type, TypeConstraint, Constraint(..), Scheme(Scheme)
  , TermN(VarN), (==>), (<|), mkVar
  )


constrain :: Env.Env -> ModuleName.Canonical -> Effects.Canonical -> IO TypeConstraint
constrain env moduleName effects =
  case effects of
    Effects.None ->
      return CSaveEnv

    Effects.Port _ ->
      return CSaveEnv

    Effects.Manager _ info ->
      constrainHelp env moduleName info


constrainHelp :: Env.Env -> ModuleName.Canonical -> Effects.Info -> IO TypeConstraint
constrainHelp env moduleName (Effects.Info tagRegion r0 r1 r2 managerType) =
  let
    task t =
      Env.getType env "Platform.Task" <| Env.getType env "Basics.Never" <| VarN t

    router msg selfMsg =
      Env.getType env "Platform.Router" <| VarN msg <| VarN selfMsg
  in
    do  v0 <- mkVar Nothing
        v1 <- mkVar Nothing
        v2 <- mkVar Nothing

        state0 <- mkVar Nothing
        state1 <- mkVar Nothing
        state2 <- mkVar Nothing

        msg1 <- mkVar Nothing
        msg2 <- mkVar Nothing

        selfMsg1 <- mkVar Nothing
        selfMsg2 <- mkVar Nothing

        let
          vars =
            [v0, v1, v2, state0, state1, state2, msg1, msg2, selfMsg1, selfMsg2]

          onEffectsType =
            router msg1 selfMsg1
            ==> addEffectArgs env moduleName managerType msg1
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
  :: Env.Env
  -> ModuleName.Canonical
  -> Effects.ManagerType
  -> Variable
  -> Type
  -> Type
addEffectArgs env moduleName managerType msg result =
  let
    toTypeName (A.A _ name) =
      ModuleName.canonicalToString moduleName ++ "." ++ name

    effectList fxName =
      Env.getType env "List" <|
        (Env.getType env (toTypeName fxName) <| VarN msg)
  in
    case managerType of
      Effects.CmdManager cmd ->
        effectList cmd ==> result

      Effects.SubManager sub ->
        effectList sub ==> result

      Effects.FxManager cmd sub ->
        effectList cmd ==> effectList sub ==> result

