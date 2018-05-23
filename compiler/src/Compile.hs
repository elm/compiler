{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( DocsFlag(..)
  , compile
  , Artifacts(..)
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type

import System.IO.Unsafe (unsafePerformIO)



-- COMPILE


type Result i a =
  Result.Result i [Warning.Warning] Error.Error a


type ImportDict =
  Map.Map N.Name ModuleName.Canonical


data Artifacts =
  Artifacts
    { _elmi :: I.Interface
    , _elmo :: Opt.Graph
    , _docs :: Maybe Docs.Module
    }


compile :: DocsFlag -> Pkg.Name -> ImportDict -> I.Interfaces -> BS.ByteString -> Result i Artifacts
compile flag pkg importDict interfaces source =
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program pkg source

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid

      let localizer = L.fromModule valid -- TODO should this be strict for GC?

      annotations <-
        runTypeInference localizer canonical

      () <-
        exhaustivenessCheck canonical

      graph <- Result.mapError (Error.Main localizer) $
        Optimize.optimize annotations canonical

      documentation <-
        genarateDocs flag canonical

      Result.ok $
        Artifacts
          { _elmi = I.fromModule annotations canonical
          , _elmo = graph
          , _docs = documentation
          }



-- TYPE INFERENCE


runTypeInference :: L.Localizer -> Can.Module -> Result i (Map.Map N.Name Can.Annotation)
runTypeInference localizer canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      Result.ok annotations

    Left errors ->
      Result.throw (Error.Type localizer errors)



-- EXHAUSTIVENESS CHECK


exhaustivenessCheck :: Can.Module -> Result i ()
exhaustivenessCheck canonical =
  case PatternMatches.check canonical of
    Left errors ->
      Result.throw (Error.Pattern errors)

    Right () ->
      Result.ok ()



-- DOCUMENTATION


data DocsFlag = YesDocs | NoDocs


genarateDocs :: DocsFlag -> Can.Module -> Result.Result i w Error.Error (Maybe Docs.Module)
genarateDocs flag modul =
  case flag of
    NoDocs ->
      Result.ok Nothing

    YesDocs ->
      Just <$> Docs.fromModule modul
