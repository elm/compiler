{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( compile
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Nitpick.TopLevelTypes as TopLevelTypes
import qualified Optimize.Module as Optimize
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
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


compile :: Pkg.Name -> ImportDict -> I.Interfaces -> BS.ByteString -> Result i (I.Interface, Opt.Graph)
compile pkg importDict interfaces source =
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program source

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid

      annotations <-
        runTypeInference canonical

      mains <- Result.mapError Error.Main $
        TopLevelTypes.check annotations canonical

      () <-
        exhaustivenessCheck canonical

      let (Optimize.Graph fields graph) = Optimize.optimize canonical

      Result.ok (I.fromModule annotations canonical, Opt.Graph mains graph fields)


runTypeInference :: Can.Module -> Result i (Map.Map N.Name Can.Annotation)
runTypeInference canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Left errors ->
      Result.throw (Error.Type errors)

    Right annotations ->
      Result.ok annotations


exhaustivenessCheck :: Can.Module -> Result i ()
exhaustivenessCheck canonical =
  case PatternMatches.check canonical of
    Left errors ->
      Result.throw (Error.Pattern errors)

    Right () ->
      Result.ok ()
