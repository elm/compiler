{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  )
  where


import Prelude hiding (init)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified System.Directory as Dir

import qualified Deps.Cache as Cache
import qualified Deps.Explorer as Explorer
import qualified Deps.Solver as Solver
import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Init as E
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: () -> () -> IO ()
run () () =
  do  reporter <- Terminal.create
      exists <- Dir.doesFileExist "elm.json"
      Task.run reporter $
        if exists then
          Task.throw (Exit.Init E.AlreadyStarted)
        else
          do  approved <- Task.getApproval question
              if approved
                then
                  do  init
                      liftIO $ putStrLn "Okay, I created it. Now read that link!"
                else
                  liftIO $ putStrLn "Okay, I did not make any changes!"


question :: D.Doc
question =
  D.stack
    [ D.fillSep
        ["Hello!"
        ,"Elm","projects","always","start","with","an",D.green "elm.json","file."
        ,"I","can","create","them!"
        ]
    , D.reflow
        "Now you may be wondering, what will be in this file? How do I add Elm files to\
        \ my project? How do I see it in the browser? How will my code grow? Do I need\
        \ more directories? What about tests? Etc."
    , D.fillSep
        ["Check","out",D.cyan (D.fromString (D.makeLink "init"))
        ,"for","all","the","answers!"
        ]
    , "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
    ]



-- INIT


init :: Task.Task ()
init =
  do  registry <- Cache.optionalUpdate

      maybeSolution <-
        Explorer.run registry $ Solver.run $ Solver.solve defaults

      case maybeSolution of
        Just solution ->
          let
            directs = Map.intersection solution defaults
            indirects = Map.difference solution defaults
          in
          liftIO $
            do  Dir.createDirectoryIfMissing True "src"
                Project.write "." $ Project.App $
                  Project.AppInfo Compiler.version ["src"] directs indirects Map.empty Map.empty

        Nothing ->
          Task.throw (Exit.Init (E.NoSolution (Map.keys defaults)))


defaults :: Map.Map Pkg.Name Con.Constraint
defaults =
  Map.fromList
    [ (Pkg.core, Con.anything)
    , (Pkg.browser, Con.anything)
    , (Pkg.html, Con.anything)
    ]
