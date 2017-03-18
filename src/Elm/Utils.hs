{-# OPTIONS_GHC -Wall #-}
module Elm.Utils
    ( (|>), (<|)
    , nearbyNames
    , isDeclaration
    ) where

import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Data.Text (Text)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

import qualified AST.Expression.Source as Source
import qualified AST.Pattern as Pattern
import qualified Parse.Helpers as Parse
import qualified Parse.Expression as Parse
import qualified Reporting.Annotation as A
import Reporting.Helpers (nearbyNames)



-- PIPES


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



-- DECL CHECKER


isDeclaration :: Text -> Maybe [Text]
isDeclaration source =
  case Parse.run Parse.definition source of
    Right (A.A _ (Source.Definition pattern _), _, _) ->
        Just (Pattern.boundVarList pattern)

    _ ->
        Nothing
