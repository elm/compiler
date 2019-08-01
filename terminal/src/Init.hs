{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  )
  where


import Prelude hiding (init)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir

import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit



-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.initToReport $
  do  exists <- Dir.doesFileExist "elm.json"
      if exists
        then return (Left Exit.InitAlreadyExists)
        else
          do  approved <- Reporting.ask question
              if approved
                then init
                else
                  do  putStrLn "Okay, I did not make any changes!"
                      return (Right ())


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
        ["Check","out",D.cyan (D.fromChars (D.makeLink "init"))
        ,"for","all","the","answers!"
        ]
    , "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
    ]



-- INIT


init :: IO (Either Exit.Init ())
init =
  do  eitherEnv <- Solver.initEnv
      case eitherEnv of
        Left problem ->
          return (Left (Exit.InitRegistryProblem problem))

        Right (Solver.Env cache _ connection registry) ->
          do  result <- Solver.verify cache connection registry defaults
              case result of
                Solver.Err exit ->
                  return (Left (Exit.InitSolverProblem exit))

                Solver.NoSolution ->
                  return (Left (Exit.InitNoSolution (Map.keys defaults)))

                Solver.NoOfflineSolution ->
                  return (Left (Exit.InitNoOfflineSolution (Map.keys defaults)))

                Solver.Ok details ->
                  let
                    solution = Map.map (\(Solver.Details vsn _) -> vsn) details
                    directs = Map.intersection solution defaults
                    indirects = Map.difference solution defaults
                  in
                  do  Dir.createDirectoryIfMissing True "src"
                      Outline.write "." $ Outline.App $
                        Outline.AppOutline V.compiler (NE.List (Outline.RelativeSrcDir "src") []) directs indirects Map.empty Map.empty
                      putStrLn "Okay, I created it. Now read that link!"
                      return (Right ())


defaults :: Map.Map Pkg.Name Con.Constraint
defaults =
  Map.fromList
    [ (Pkg.core, Con.anything)
    , (Pkg.browser, Con.anything)
    , (Pkg.html, Con.anything)
    ]
