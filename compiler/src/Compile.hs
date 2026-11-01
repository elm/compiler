{-# LANGUAGE Rank2Types #-}
module Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Optimized as Opt
import qualified AST.Prim.Mode as Mode
import qualified AST.Prim.Module as Module
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Reporting.Error as E
import qualified Reporting.Render.Type as RT
import qualified Type.Check as Type



-- COMPILE


data Artifacts =
  Artifacts Tpd.Module I.Interface Opt.LocalGraph


compile :: Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> IO (Either E.Errors Artifacts)
compile mode ifaces modul =
  run $
  do  can         <- canonicalize mode ifaces modul
      (iface,tpd) <- typeCheck modul can
      ()          <- nitpick tpd
      objects     <- optimize modul iface tpd
      return (Artifacts tpd iface objects)



-- PHASES


canonicalize :: Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> Task Can.Module
canonicalize mode ifaces modul =
  Task $ \ok err ->
    case Canonicalize.canonicalize mode ifaces modul of
      Canonicalize.Result k ->
        k []
          (\_ a -> ok a)
          (\_ x -> err (E.BadNames x))


typeCheck :: Src.Module -> Can.Module -> Task (I.Interface, Tpd.Module)
typeCheck src can =
  Task $ \ok err ->
    do  outcome <- Type.check can
        case outcome of
          Type.Ok (iface,tpd) -> ok (iface, tpd)
          Type.BadType    xs  -> err $ E.BadTypes (RT.fromSourceModule src) xs
          Type.BadNitpick xs  -> err $ E.BadNitpick xs


nitpick :: Tpd.Module -> Task ()
nitpick tpd =
  Task $ \ok err ->
    case PatternMatches.check tpd of
      Right () -> ok ()
      Left  xs -> err $ E.BadPatterns xs


optimize :: Src.Module -> I.Interface -> Tpd.Module -> Task Opt.LocalGraph
optimize modul (I.Interface _ annotations _ _) typed =
  Task $ \ok err ->
    case Optimize.optimize annotations typed of
      Optimize.Result k ->
        k []
          (\_ opt -> ok opt)
          (\_ xs  -> err (E.BadMains (RT.fromSourceModule modul) xs))



-- TASK


newtype Task a =
  Task (forall r. (a -> IO r) -> (E.Errors -> IO r) -> IO r)


run :: Task a -> IO (Either E.Errors a)
run (Task k) =
  k (return . Right) (return . Left)


instance Functor Task where
  {-# INLINE fmap #-}
  fmap func (Task k) =
    Task $ \ok err ->
      k (ok . func) err


instance Applicative Task where
  {-# INLINE pure #-}
  pure a =
    Task $ \ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Task kFunc) (Task kArg) =
    Task $ \ok err ->
      let
        okF func = kArg (ok . func) err
      in
      kFunc okF err


instance Monad Task where
  {-# INLINE (>>=) #-}
  (>>=) (Task kA) callback =
    Task $ \ok err ->
      let
        okA a =
          case callback a of
            Task kB -> kB ok err
      in
      kA okA err

