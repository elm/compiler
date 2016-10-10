{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Utils
    ( (|>), (<|)
    , run, unwrappedRun
    , CommandError(..)
    , isDeclaration
    ) where

import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import qualified Data.List as List
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

import qualified AST.Expression.Source as Source
import qualified AST.Pattern as Pattern
import qualified Parse.Helpers as Parse
import qualified Parse.Expression as Parse
import qualified Reporting.Annotation as A


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


-- RUN EXECUTABLES

data CommandError
    = MissingExe String
    | CommandFailed String String


{-| Run a command, throw an error if the command is not found or if something
goes wrong.
-}
run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run command args =
  do  result <- liftIO (unwrappedRun command args)
      case result of
        Right out ->
          return out

        Left err ->
          throwError (context (message err))
  where
    context msg =
      "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ msg

    message err =
      case err of
        CommandFailed stdout stderr ->
          stdout ++ stderr

        MissingExe msg ->
          msg


unwrappedRun :: String -> [String] -> IO (Either CommandError String)
unwrappedRun command args =
  do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
      return $
          case exitCode of
            ExitSuccess ->
              Right stdout

            ExitFailure code ->
              if code == 127 then
                Left (missingExe command)  -- UNIX

              else if code == 9009 then
                Left (missingExe command)  -- Windows

              else
                Left (CommandFailed stdout stderr)


missingExe :: String -> CommandError
missingExe command =
  MissingExe $
    "Could not find command `" ++ command ++ "`. Do you have it installed?\n\
    \    Can it be run from anywhere? Is it on your PATH?"



-- DECL CHECKER

isDeclaration :: String -> Maybe String
isDeclaration string =
  case Parse.iParse Parse.def string of
    Right (A.A _ (Source.Definition pattern _)) ->
        Just (List.intercalate "$" (Pattern.boundVarList pattern))

    _ ->
        Nothing
