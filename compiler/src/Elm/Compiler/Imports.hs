{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( defaults
  )
  where


import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Reporting.Annotation as A



-- DEFAULTS


defaults :: [Src.Import]
defaults =
  [ import_ Module.basics        Nothing                 Src.Open
  , import_ Module.debug         Nothing                 closed
  , import_ Module.list          Nothing                 (operator Op.cons)
  , import_ Module.maybe         Nothing                 (typeOpen T.maybe)
  , import_ Module.result        Nothing                 (typeOpen T.result)
  , import_ Module.string        Nothing                 (typeClosed T.string)
  , import_ Module.char          Nothing                 (typeClosed T.char)
  , import_ Module.tuple         Nothing                 closed
  , import_ Module.platform      Nothing                 (typeClosed T.program)
  , import_ Module.platform_cmd (Just Module.prefix_cmd) (typeClosed T.cmd)
  , import_ Module.platform_sub (Just Module.prefix_sub) (typeClosed T.sub)
  ]


import_ :: Module.Name -> Maybe Module.Prefix -> Src.Exposing -> Src.Import
import_ name maybeAlias exposing =
  Src.Import (A.At A.zero name) maybeAlias exposing



-- EXPOSING


closed :: Src.Exposing
closed =
  Src.Explicit []


typeOpen :: T.Name -> Src.Exposing
typeOpen name =
  Src.Explicit [ Src.Upper (A.At A.zero name) (Src.Public A.zero) ]


typeClosed :: T.Name -> Src.Exposing
typeClosed name =
  Src.Explicit [ Src.Upper (A.At A.zero name) Src.Private ]


operator :: Op.Name -> Src.Exposing
operator op =
  Src.Explicit [ Src.Operator A.zero op ]
