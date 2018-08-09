module Generate.Nitpick
  ( findDebugUses
  )
  where


import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Name as N



-- FIND DEBUG USES


findDebugUses :: Pkg.Name -> Opt.Graph -> [N.Name]
findDebugUses pkg (Opt.Graph _ graph _) =
  Set.toList $ Map.foldrWithKey (addDebugUses pkg) Set.empty graph


addDebugUses :: Pkg.Name -> Opt.Global -> Opt.Node -> Set.Set N.Name -> Set.Set N.Name
addDebugUses here (Opt.Global (ModuleName.Canonical pkg home) _) node uses =
  if pkg == here && nodeHasDebug node then
    Set.insert home uses
  else
    uses


nodeHasDebug :: Opt.Node -> Bool
nodeHasDebug node =
  case node of
    Opt.Define expr _           -> hasDebug expr
    Opt.DefineTailFunc _ expr _ -> hasDebug expr
    Opt.Ctor _ _                -> False
    Opt.Enum _                  -> False
    Opt.Box                     -> False
    Opt.Link _                  -> False
    Opt.Cycle _ vs fs _         -> any (hasDebug . snd) vs || any defHasDebug fs
    Opt.Manager _               -> False
    Opt.Kernel _ _              -> False
    Opt.PortIncoming expr _     -> hasDebug expr
    Opt.PortOutgoing expr _     -> hasDebug expr


hasDebug :: Opt.Expr -> Bool
hasDebug expression =
  case expression of
    Opt.Bool _           -> False
    Opt.Chr _            -> False
    Opt.Str _            -> False
    Opt.Int _            -> False
    Opt.Float _          -> False
    Opt.VarLocal _       -> False
    Opt.VarGlobal _      -> False
    Opt.VarEnum _ _      -> False
    Opt.VarBox _         -> False
    Opt.VarCycle _ _     -> False
    Opt.VarDebug _ _ _ _ -> True
    Opt.VarKernel _ _    -> False
    Opt.List exprs       -> any hasDebug exprs
    Opt.Function _ expr  -> hasDebug expr
    Opt.Call e es        -> hasDebug e || any hasDebug es
    Opt.TailCall _ args  -> any (hasDebug . snd) args
    Opt.If conds finally -> any (\(c,e) -> hasDebug c || hasDebug e) conds || hasDebug finally
    Opt.Let def body     -> defHasDebug def || hasDebug body
    Opt.Destruct _ expr  -> hasDebug expr
    Opt.Case _ _ d jumps -> deciderHasDebug d || any (hasDebug . snd) jumps
    Opt.Accessor _       -> False
    Opt.Access r _       -> hasDebug r
    Opt.Update r fs      -> hasDebug r || any hasDebug fs
    Opt.Record fs        -> any hasDebug fs
    Opt.Unit             -> False
    Opt.Tuple a b c      -> hasDebug a || hasDebug b || maybe False hasDebug c
    Opt.Shader _ _ _     -> False


defHasDebug :: Opt.Def -> Bool
defHasDebug def =
  case def of
    Opt.Def _ expr       -> hasDebug expr
    Opt.TailDef _ _ expr -> hasDebug expr


deciderHasDebug :: Opt.Decider Opt.Choice -> Bool
deciderHasDebug decider =
  case decider of
    Opt.Leaf (Opt.Inline expr)  -> hasDebug expr
    Opt.Leaf (Opt.Jump _)       -> False
    Opt.Chain _ success failure -> deciderHasDebug success || deciderHasDebug failure
    Opt.FanOut _ tests fallback -> any (deciderHasDebug . snd) tests || deciderHasDebug fallback



-- TODO: FIND GLOBALLY UNUSED DEFINITIONS?
-- TODO: FIND PACKAGE USAGE STATS? (e.g. elm/core = 142, author/project = 2, etc.)
