module Nitpick.Debug
  ( hasDebugUses
  )
  where


import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyMap as NEM

import qualified AST.Optimized as Opt



-- HAS DEBUG USES


hasDebugUses :: Opt.LocalGraph -> Bool
hasDebugUses (Opt.LocalGraph _ graph _) =
  Map.any nodeHasDebug graph


nodeHasDebug :: Opt.Node -> Bool
nodeHasDebug node =
  case node of
    Opt.Define e _             -> hasDebug e
    Opt.DefineTailFunc _ _ e _ -> hasDebug e
    Opt.Variant _ _            -> False
    Opt.Enum _                 -> False
    Opt.Vector _               -> False
    Opt.Box                    -> False
    Opt.Void                   -> False
    Opt.Link _                 -> False
    Opt.Cycle _ _ vs fs _      -> any (hasDebug . snd) vs || any defHasDebug fs
    Opt.Manager _              -> False
    Opt.Kernel _ _             -> False
    Opt.PortIncoming e _       -> hasDebug e
    Opt.PortOutgoing e _       -> hasDebug e


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
    Opt.List es          -> any hasDebug es
    Opt.Function _ _ e   -> hasDebug e
    Opt.Call f e es      -> hasDebug f || hasDebug e || any hasDebug es
    Opt.TailCall _ x xs  -> tailAssignHasDebug x || any tailAssignHasDebug xs
    Opt.If conds finally -> any (\(c,e) -> hasDebug c || hasDebug e) conds || hasDebug finally
    Opt.Let def body     -> defHasDebug def || hasDebug body
    Opt.Destruct _ expr  -> hasDebug expr
    Opt.Case _ _ d jumps -> deciderHasDebug d || any (hasDebug . snd) jumps
    Opt.Accessor _       -> False
    Opt.Access r _       -> hasDebug r
    Opt.Update r fs      -> hasDebug r || NEM.any hasDebug fs
    Opt.Record fs        -> NEM.any hasDebug fs
    Opt.Empty            -> False
    Opt.Unit             -> False
    Opt.Pair   a b       -> hasDebug a || hasDebug b
    Opt.Triple a b c     -> hasDebug a || hasDebug b || hasDebug c
    Opt.Shader _ _ _     -> False


tailAssignHasDebug :: Opt.TailAssign -> Bool
tailAssignHasDebug (Opt.TailAssign _ e) =
  hasDebug e


defHasDebug :: Opt.Def -> Bool
defHasDebug def =
  case def of
    Opt.Def _ e         -> hasDebug e
    Opt.TailDef _ _ _ e -> hasDebug e


deciderHasDebug :: Opt.Decider Opt.Choice -> Bool
deciderHasDebug decider =
  case decider of
    Opt.Leaf (Opt.Inline expr)  -> hasDebug expr
    Opt.Leaf (Opt.Jump _)       -> False
    Opt.Chain _ success failure -> deciderHasDebug success || deciderHasDebug failure
    Opt.FanOut _ tests fallback -> any (deciderHasDebug . snd) tests || deciderHasDebug fallback



-- TODO: FIND GLOBALLY UNUSED DEFINITIONS?
-- TODO: FIND PACKAGE USAGE STATS? (e.g. elm/core = 142, author/project = 2, etc.)
