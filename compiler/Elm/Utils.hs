{-# OPTIONS_GHC -Wall #-}
module Elm.Utils ((|>), (<|), getAsset) where

import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)


{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 0 <|
infixl 0 |>


{-| Get the absolute path to a data file. If you install with cabal it will look
-}
getAsset :: String -> (FilePath -> IO FilePath) -> FilePath -> IO FilePath
getAsset project getDataFileName name =
  do  path <- getDataFileName name
      exists <- doesFileExist path
      if exists
        then return path
        else do
          environment <- tryIOError (getEnv "ELM_HOME")
          case environment of
            Right env ->
                return (env </> project </> name)

            Left _ ->
                fail (errorNotFound name)


errorNotFound :: FilePath -> String
errorNotFound name =
    unlines
    [ "Unable to find the ELM_HOME environment variable when searching"
    , "for the " ++ name ++ " file."
    , ""
    , "If you installed Elm Platform with the Mac or Windows installer, it looks like"
    , "ELM_HOME was not set automatically. Look up how to set environment variables"
    , "on your platform and set ELM_HOME to the directory that contains Elm's static"
    , "files:"
    , ""
    , "  * On Mac it is /usr/local/share/elm"
    , "  * On Windows it is one of the following:"
    , "      C:/Program Files/Elm Platform/0.13/share"
    , "      C:/Program Files (x86)/Elm Platform/0.13/share"
    , ""
    , "If it seems like a more complex issue, please report it here:"
    , "    <https://github.com/elm-lang/elm-platform/issues>"
    ]