{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Environment
  ( Optimizer, run
  , Env
  , getVariantDict
  , getHome
  , getTailCall, setTailCall
  , freshName
  , register
  , registerEffects
  )
  where

import qualified Control.Monad.State as State
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT



-- ENVIRONMENT


data Env =
  Env
    { _read_only_variants :: !DT.VariantDict
    , _read_only_home :: !ModuleName.Canonical
    , _uid :: !Int
    , _deps :: Set.Set Var.Global
    , _effects :: Maybe Effects.ManagerType
    , _has_tail_call :: !Bool
    }



-- OPTIMIZER


type Optimizer a =
    State.State Env a


run
  :: DT.VariantDict
  -> ModuleName.Canonical
  -> Optimizer (Text, Opt.Def)
  -> (Text, Opt.Decl)
run variantDict home optimizer =
  let
    ((name, def), Env _ _ _ deps fx _) =
      State.runState optimizer $
        Env variantDict home 0 Set.empty Nothing False
  in
    ( name, Opt.Decl deps fx def )



-- READ ONLY


getVariantDict :: Optimizer DT.VariantDict
getVariantDict =
  State.gets _read_only_variants


getHome :: Optimizer ModuleName.Canonical
getHome =
  State.gets _read_only_home



-- TAIL CALLS


getTailCall :: Optimizer Bool
getTailCall =
  State.gets _has_tail_call


setTailCall :: Bool -> Optimizer ()
setTailCall bool =
  do  env <- State.get
      State.put (env { _has_tail_call = bool })



-- FRESH VARIABLES


freshName :: Optimizer Text
freshName =
  do  (Env vs home uid deps fx htc) <- State.get
      State.put (Env vs home (uid + 1) deps fx htc)
      return ("_p" <> Text.pack (show uid))



-- REGISTER DEPENDENCIES


register :: Var.Canonical -> Optimizer ()
register (Var.Canonical home name) =
  case home of
    Var.BuiltIn ->
      return ()

    Var.Module realHome ->
      State.modify (registerHelp (Var.Global realHome name))

    Var.TopLevel realHome ->
      State.modify (registerHelp (Var.Global realHome name))

    Var.Local ->
      return ()


registerHelp :: Var.Global -> Env -> Env
registerHelp dep (Env vs home uid deps fx htc) =
  Env vs home uid (Set.insert dep deps) fx htc



-- REGISTER EFFECTS


registerEffects :: Effects.ManagerType -> Optimizer Effects.ManagerType
registerEffects manager =
  do  State.modify (registerEffectsHelp manager)
      return manager


registerEffectsHelp :: Effects.ManagerType -> Env -> Env
registerEffectsHelp manager (Env vs home uid deps _ htc) =
  Env vs home uid deps (Just manager) htc
