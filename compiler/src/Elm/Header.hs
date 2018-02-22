{-# OPTIONS_GHC -Wall #-}
module Elm.Header
  ( Tag(..)
  , parse
  )
  where


import qualified Data.ByteString as B

import qualified AST.Source as Src
import qualified Elm.Compiler.Module as M
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Parse.Primitives as Parser
import qualified Parse.Module as Module
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error



-- HEADER TAGS


data Tag = Normal | Effect | Port



-- PARSE


parse :: Pkg.Name -> B.ByteString -> Either Error.Error (Maybe (Tag, M.Raw), [M.Raw])
parse pkgName sourceCode =
  let
    headerParser =
      Module.module_ pkgName (return ())
  in
  case Parser.run headerParser sourceCode of
    Right (Src.Module header imports _) ->
      Right
        ( fmap simplifyHeader header
        , map getName imports
        )

    Left err ->
      Left (Error.Syntax err)


getName :: Src.Import -> N.Name
getName (Src.Import (A.At _ name) _ _) =
  name



-- TO HEADER


simplifyHeader :: Src.Header -> (Tag, N.Name)
simplifyHeader (Src.Header name effects _ _) =
  ( toTag effects, name )


toTag :: Src.Effects -> Tag
toTag effects =
  case effects of
    Src.NoEffects ->
      Normal

    Src.Ports _ ->
      Port

    Src.Manager _ _ ->
      Effect
