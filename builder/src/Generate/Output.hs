{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Output
  ( Mode(..)
  , Mode.Target(..)
  , generate
  , generateReplFile
  , noDebugUsesInPackage
  , Output(..)
  , output
  )
  where


import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg

import qualified AST.Module.Name as ModuleName
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified Generate.Functions as Functions
import qualified Generate.Html as Html
import qualified Generate.Nitpick as Nitpick
import qualified Generate.JavaScript.Mode as Mode
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Make as E
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths
import Terminal.Args (Parser(..))



-- GENERATE


data Mode = Debug | Dev | Prod


generate
  :: Mode
  -> Mode.Target
  -> Maybe Output
  -> Summary.Summary
  -> Crawl.Result
  -> Map.Map Module.Raw Compiler.Artifacts
  -> Task.Task ()
generate mode target maybeOutput summary graph@(Crawl.Graph args locals _ _ _) artifacts =
  case args of
    Args.Pkg _ ->
      return ()

    Args.Roots name names ->
      do  objectGraph <- organize summary graph

          realMode <-
            case mode of
              Debug ->
                do  interfaces <- getInterfaces summary locals artifacts
                    return $ Mode.debug target interfaces

              Dev ->
                return $ Mode.dev target

              Prod ->
                do  noDebugUses summary objectGraph
                    return $ Mode.prod target objectGraph

          generateMonolith realMode maybeOutput summary objectGraph (name:names)


getInterfaces
  :: Summary.Summary
  -> Map.Map Module.Raw a
  -> Map.Map Module.Raw Compiler.Artifacts
  -> Task.Task I.Interfaces
getInterfaces (Summary.Summary root project _ interfaces _) locals artifacts =
  let
    pkg =
      Project.getName project

    addArtifact home (Compiler.Artifacts elmi _ _) ifaces =
      (ModuleName.Canonical pkg home, elmi) : ifaces

    addInterface home iface ifaces =
      (ModuleName.Canonical pkg home, iface) : ifaces

    readInterface home _ =
      IO.readBinary (Paths.elmi root home)
  in
  do  cached <- Map.traverseWithKey readInterface (Map.difference locals artifacts)
      return $
        Map.union interfaces $ Map.fromList $
          Map.foldrWithKey addInterface (Map.foldrWithKey addArtifact [] artifacts) cached



-- GENERATE MONOLITH


generateMonolith
  :: Mode.Mode
  -> Maybe Output
  -> Summary.Summary
  -> Obj.Graph
  -> [Module.Raw]
  -> Task.Task ()
generateMonolith mode maybeOutput (Summary.Summary _ project _ _ _) graph rootNames =
  do  let pkg = Project.getName project
      let roots = map (Module.Canonical pkg) rootNames
      case Obj.generate mode graph roots of
        Obj.None ->
          return ()

        Obj.Some name _names builder ->
          let
            monolith =
              "(function(scope){\n'use strict';"
              <> Functions.functions <> builder <> "}(this));"
          in
          liftIO $
          case maybeOutput of
            Nothing ->
              IO.writeBuilder "index.html" (Html.sandwich name monolith)

            Just output_ ->
              case output_ of
                None ->
                  return ()

                JavaScript maybeDir fileName ->
                  do  path <- toWritablePath maybeDir fileName
                      IO.writeBuilder path monolith

                HtmlBuilder mvar ->
                  putMVar mvar (Html.sandwich name monolith)

                Html maybeDir fileName ->
                  do  path <- toWritablePath maybeDir fileName
                      IO.writeBuilder path (Html.sandwich name monolith)



-- GENERATE REPL MONOLITH


generateReplFile
  :: Bool
  -> L.Localizer
  -> Summary.Summary
  -> Crawl.Result
  -> I.Interface
  -> N.Name
  -> Task.Task FilePath
generateReplFile noColors localizer summary@(Summary.Summary _ project _ _ _) graph iface name =
  do
      objectGraph <- organize summary graph

      let home = Module.Canonical (Project.getName project) N.replModule
      let builder = Obj.generateForRepl (not noColors) localizer objectGraph iface home name

      liftIO $ IO.writeBuilder (Paths.temp "js") $
        replRecovery <> "(function(){\n'use strict';" <> Functions.functions <> builder <> "}());"

      return (Paths.temp "js")


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



-- NO DEBUG USES


noDebugUses :: Summary.Summary -> Obj.Graph -> Task.Task ()
noDebugUses (Summary.Summary _ project _ _ _) graph =
  case Nitpick.findDebugUses (Project.getName project) graph of
    [] ->
      return ()

    m:ms ->
      Task.throw (Exit.Make (E.CannotOptimizeDebugValues m ms))


noDebugUsesInPackage :: Summary.Summary -> Crawl.Result -> Task.Task ()
noDebugUsesInPackage summary graph =
  noDebugUses summary =<< organize summary graph



-- OUTPUT


data Output
  = None
  | Html (Maybe FilePath) FilePath
  | JavaScript (Maybe FilePath) FilePath
  | HtmlBuilder (MVar B.Builder)


toWritablePath :: Maybe FilePath -> FilePath -> IO FilePath
toWritablePath maybeDir fileName =
  case maybeDir of
    Nothing ->
      do  return fileName

    Just dir ->
      do  Dir.createDirectoryIfMissing True dir
          return (dir </> fileName)



-- OUTPUT PARSER


output :: Parser Output
output =
  Parser
    { _singular = "output file"
    , _plural = "output files"
    , _parser = parseOutput
    , _suggest = \_ -> return []
    , _examples = exampleOutput
    }


parseOutput :: String -> Maybe Output
parseOutput string =
    if string == "/dev/null" || string == "NUL" || string == "$null" then
      Just None

    else if FP.takeExtension string == ".html" && length string > 5 then
      splitOutput Html string

    else if FP.takeExtension string == ".js" && length string > 3 then
      splitOutput JavaScript string

    else
      Nothing


splitOutput :: (Maybe FilePath -> FilePath -> Output) -> String -> Maybe Output
splitOutput toOutput string =
  case FP.splitPath string of
    [] ->
      Nothing

    [name] ->
      Just $ toOutput Nothing name

    segments ->
      Just $ toOutput (Just (FP.joinPath (init segments))) (last segments)


exampleOutput :: String -> IO [String]
exampleOutput _ =
  return [ "elm.js", "index.html", "/dev/null" ]
