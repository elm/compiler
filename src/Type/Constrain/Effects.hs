{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Effects (constrain) where

import qualified Data.Map as Map

import qualified AST.Effects as Fx
import qualified AST.Module.Name as ModuleName
import qualified Reporting.Annotation as A
import qualified Type.Environment as Env
import Type.Type
  ( Variable, Type, TypeConstraint, Constraint(..), Scheme(Scheme)
  , TermN(VarN), (==>), (<|), mkVar
  )


constrain
  :: Env.Environment
  -> ModuleName.Canonical
  -> Fx.Effects
  -> IO TypeConstraint
constrain env moduleName effects =
  case effects of
    Fx.None ->
      return CSaveEnv

    Fx.Foreign ->
      return CSaveEnv

    Fx.Effect info ->
      constrainHelp env moduleName info


constrainHelp
  :: Env.Environment
  -> ModuleName.Canonical
  -> Fx.Info
  -> IO TypeConstraint
constrainHelp env moduleName (Fx.Info tagRegion r0 r1 r2 managerType) =
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
            , CEqual (error "TODO 1") r0 (VarN v0) (task state0)
            , CEqual (error "TODO 2") r1 (VarN v1) onEffectsType
            , CEqual (error "TODO 3") r2 (VarN v2) onSelfMsgType
            , CEqual (error "TODO 4") r1 (VarN state0) (VarN state1)
            , CEqual (error "TODO 5") r2 (VarN state0) (VarN state2)
            , CEqual (error "TODO 6") r2 (VarN selfMsg1) (VarN selfMsg2)
            ]

        return $
          CLet [Scheme [] vars (CAnd constrains) Map.empty] CSaveEnv


addEffectArgs
  :: Env.Environment
  -> ModuleName.Canonical
  -> Fx.ManagerType
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
      Fx.CmdManager cmd ->
        effectList cmd ==> result

      Fx.SubManager sub ->
        effectList sub ==> result

      Fx.FxManager cmd sub ->
        effectList cmd ==> effectList sub ==> result

