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
  , indirectly
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
    , _deps :: Deps
    , _effects :: Maybe Effects.ManagerType
    , _has_tail_call :: !Bool
    }



-- DEPS


data Deps
  = Direct { _direct :: Set.Set Var.Global, _indirect :: Set.Set Var.Global }
  | Indirect (Set.Set Var.Global)


emptyDeps :: Deps
emptyDeps =
  Direct Set.empty Set.empty


insertDep :: Var.Global -> Deps -> Deps
insertDep var deps =
  case deps of
    Direct direct indirect ->
      Direct (Set.insert var direct) indirect

    Indirect indirect ->
      Indirect (Set.insert var indirect)


unionDeps :: Set.Set Var.Global -> Deps -> Deps
unionDeps vars deps =
  case deps of
    Direct direct indirect ->
      Direct (Set.union vars direct) indirect

    Indirect indirect ->
      Indirect (Set.union vars indirect)



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
    ((name, def), Env _ _ _ (Direct direct indirect) fx _) =
      State.runState optimizer $
        Env variantDict home 0 emptyDeps Nothing False
  in
    ( name, Opt.Decl direct indirect fx def )



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


register :: Var.Global -> Optimizer ()
register global =
  State.modify (registerHelp global)


registerHelp :: Var.Global -> Env -> Env
registerHelp dep (Env vs home uid deps fx htc) =
  Env vs home uid (insertDep dep deps) fx htc


indirectly :: Optimizer a -> Optimizer a
indirectly optimizer =
  do  (Env vs home uid deps fx htc) <- State.get
      case deps of
        Indirect _ ->
          optimizer

        Direct direct indirect ->
          do  State.put (Env vs home uid (Indirect indirect) fx htc)
              result <- optimizer
              (Env vs_ home_ uid_ (Indirect indirect_) fx_ htc_) <- State.get
              State.put (Env vs_ home_ uid_ (Direct direct indirect_) fx_ htc_)
              return result



-- REGISTER EFFECTS


registerEffects :: ModuleName.Canonical -> Effects.ManagerType -> Optimizer Effects.ManagerType
registerEffects home manager =
  do  (Env vs hm uid deps _ htc) <- State.get
      let newDeps = unionDeps (getEffectDeps home manager) deps
      State.put (Env vs hm uid newDeps (Just manager) htc)
      return manager


getEffectDeps :: ModuleName.Canonical -> Effects.ManagerType -> Set.Set Var.Global
getEffectDeps home manager =
  Set.fromList $ map (Var.Global home) $
    case manager of
      Effects.Cmds ->
        [ "init", "onEffects", "onSelfMsg", "cmdMap" ]

      Effects.Subs ->
        [ "init", "onEffects", "onSelfMsg", "subMap" ]

      Effects.Both ->
        [ "init", "onEffects", "onSelfMsg", "cmdMap", "subMap" ]
