{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Output
  ( generate
  , generateReplFile
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Builder as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Monoid ((<>))
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Name as N
import qualified Elm.Package as Pkg

import qualified Elm.Project.Flags as Flags
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified Generate.Functions as Functions
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- GENERATE


generate :: Flags.Options -> Summary.Summary -> Crawl.Result -> Task.Task ()
generate options summary graph@(Crawl.Graph args _ _ _ _) =
  case args of
    Args.Pkg _ ->
      return ()

    Args.Roots names ->
      generateMonolith options summary graph (NonEmpty.toList names)



-- GENERATE MONOLITH


generateMonolith :: Flags.Options -> Summary.Summary -> Crawl.Result -> [Module.Raw] -> Task.Task ()
generateMonolith (Flags.Options debug target output) summary@(Summary.Summary _ project _ ifaces _) graph names =
  do
      objectGraph <- organize summary graph
      let pkg = Project.getName project
      let roots = map (Module.Canonical pkg) names
      let (Right builder) = Obj.generate debug target ifaces objectGraph roots
      let monolith =
            "(function(scope){\n'use strict';" <> Functions.functions <> builder <> "}(this));"

      liftIO $
        case output of
          Nothing ->
            IO.writeBuilder "elm.js" monolith

          Just Flags.None ->
            return ()

          Just (Flags.Custom maybeDir fileName) ->
            do  path <- Flags.safeCustomPath maybeDir fileName
                IO.writeBuilder path monolith



-- GENERATE REPL MONOLITH


generateReplFile :: Summary.Summary -> Crawl.Result -> N.Name -> Task.Task FilePath
generateReplFile summary@(Summary.Summary _ project _ ifaces _) graph name =
  do
      objectGraph <- organize summary graph

      let home = Module.Canonical (Project.getName project) "ElmRepl"
      let builder = Obj.generateForRepl ifaces objectGraph home name

      liftIO $ IO.writeBuilder Paths.temp $
        replRecovery <> "(function(){\n'use strict';" <> Functions.functions <> builder <> "}());"

      return Paths.temp


replRecovery :: B.Builder
replRecovery =
  "process.on('uncaughtException', function(err) { process.stderr.write(err.toString()); process.exit(1); });"



-- ORGANIZE


organize :: Summary.Summary -> Crawl.Result -> Task.Task Obj.Graph
organize (Summary.Summary root _ _ _ deps) (Crawl.Graph _ locals _ _ _) =
  do  localObjs <- Obj.unions <$> traverse (loadModuleObj root) (Map.keys locals)
      foreignObjs <- Obj.unions <$> traverse loadPackageObj (Map.toList deps)
      return (Obj.union localObjs foreignObjs)


loadModuleObj :: FilePath -> Module.Raw -> Task.Task Obj.Graph
loadModuleObj root name =
  IO.readBinary (Paths.elmo root name)


loadPackageObj :: ( Pkg.Name, (Pkg.Version, deps) ) -> Task.Task Obj.Graph
loadPackageObj ( name, (version,_) ) =
  do  dir <- Task.getPackageCacheDirFor name version
      IO.readBinary (dir </> "objs.dat")

