{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Either (lefts, rights)
import Data.List (intersect, intercalate)
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs
import Text.Blaze.Html.Renderer.String (renderHtml)

import Ast
import Initialize
import CompileToJS
import GenerateHtml
import Paths_Elm


data ELM =
    ELM { make :: Bool
        , files :: [FilePath]
        , runtime :: Maybe FilePath
--        , separate_js :: Bool
--        , only_js :: Bool
--        , verbose_js :: Bool
        }
    deriving (Data,Typeable,Show,Eq)

elm = ELM { make = False &= help "automatically compile dependencies."
          , files = def &= args &= typ "FILES"
          , runtime = Nothing &= typ "FILE" &=
            help "Specify a custom location for Elm's runtime system."
--          , separate_js = False &= help "Compile to separate HTML and JS files."
--          , only_js = False &= help "Compile only to JavaScript."
--          , verbose_js = False &= help "Produce JavaScript that may be easier to debug."
          } &=
    help "Compile Elm programs to HTML, CSS, and JavaScript." &=
    summary "The Elm Compiler v0.3.0, (c) Evan Czaplicki"

main = do
  args <- cmdArgs elm
  mini <- getDataFileName "elm-runtime-0.3.0.js"
  compileArgs mini args

compileArgs mini (ELM _ [] _) =
    putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
compileArgs mini (ELM make files rtLoc) =
    mapM_ (fileToHtml $ fromMaybe mini rtLoc) files

fileToHtml rtLoc file = do
  ems <- getModules [] file
  case ems of
    Left err -> putStrLn $ "Error while compiling " ++ file ++ ":\n" ++ err
    Right ms ->
        let name = reverse . tail . dropWhile (/='.') $ reverse file in
        writeFile (name ++ ".html") . renderHtml $
        modulesToHtml rtLoc ms


getModules :: [String] -> FilePath -> IO (Either String [Module])
getModules uses file = do
  code <- readFile file
  case initialize code of
    Left err -> return . Left $ "Error in " ++ file ++ ":\n" ++ err
    Right modul@(Module _ _ imports _) ->
        let imps = filter (`notElem` builtInModules) $ map fst imports in
        case intersect uses imps of
          x:_ -> return . Left $ "Error: Cyclic dependency. Module " ++
                                 x ++ " depends on itself."
          [] -> do
            ems <- mapM (getModules (uses ++ imps) . toFilePath) imps
            return $ case lefts ems of
              [] -> Right $ concat (rights ems) ++ [modul]
              errs -> Left $ intercalate "\n" errs

toFilePath :: String -> FilePath
toFilePath modul = map (\c -> if c == '.' then '/' else c) modul ++ ".elm"

builtInModules =
    concat [ map ("Data."++)   [ "List", "Char", "Maybe" ]
           , map ("Signal."++) [ "Mouse", "Keyboard.Raw"
                               , "Window", "Time", "HTTP", "Input", "Random" ]
           , [ "Element", "Text", "Color", "Line" ]
           , [ "Prelude" ]
           ]