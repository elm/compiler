{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Type
  ( Context(..)
  , lambda
  , apply
  , tuple
  , record
  , recordSnippet
  , srcToDoc
  , canToDoc
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc ( Doc, (<+>), (<>) )



-- TO DOC


data Context
  = None
  | Func
  | App


lambda :: Context -> Doc -> Doc -> [Doc] -> Doc
lambda context arg1 arg2 args =
  let
    lambdaDoc =
      D.sep (arg1 : map ("->" <+>) (arg2:args))
  in
  case context of
    None -> lambdaDoc
    Func -> D.cat [ "(", lambdaDoc, ")" ]
    App  -> D.cat [ "(", lambdaDoc, ")" ]


apply :: Context -> Doc -> [Doc] -> Doc
apply context name args =
  case args of
    [] ->
      name

    _:_ ->
      let
        applyDoc =
          D.hang 4 (D.sep (name : args))
      in
      case context of
        App  -> D.cat [ "(", applyDoc, ")" ]
        Func -> applyDoc
        None -> applyDoc


tuple :: Doc -> Doc -> [Doc] -> Doc
tuple a b cs =
  let
    entries =
      zipWith (<+>) ("(" : repeat ",") (a:b:cs)
  in
  D.sep [ D.cat entries, ")" ]


record :: [(Doc, Doc)] -> Maybe Doc -> Doc
record entries maybeExt =
  case (map entryToDoc entries, maybeExt) of
    ([], Nothing) ->
        "{}"

    (fields, Nothing) ->
        D.sep
          [ D.cat (zipWith (<+>) ("{" : repeat ",") fields)
          , "}"
          ]

    (fields, Just ext) ->
        D.sep
          [ D.hang 4 $ D.sep $
              [ "{" <+> ext
              , D.cat (zipWith (<+>) ("|" : repeat ",") fields)
              ]
          , "}"
          ]


entryToDoc :: (Doc, Doc) -> Doc
entryToDoc (fieldName, fieldType) =
  D.hang 4 (D.sep [ fieldName <+> ":", fieldType ])


recordSnippet :: (Doc, Doc) -> [(Doc, Doc)] -> Doc
recordSnippet entry entries =
  let
    field  = "{" <+> entryToDoc entry
    fields = zipWith (<+>) (repeat ",") (map entryToDoc entries ++ ["..."])
  in
  D.sep [ D.cat (field:fields), "}" ]



-- SOURCE TYPE TO DOC


srcToDoc :: Context -> Src.Type -> Doc
srcToDoc context (A.At _ tipe) =
  case tipe of
    Src.TLambda arg1 result ->
      let
        (arg2, rest) = collectSrcArgs result
      in
      lambda context
        (srcToDoc Func arg1)
        (srcToDoc Func arg2)
        (map (srcToDoc Func) rest)

    Src.TVar name ->
      D.fromName name

    Src.TType _ name args ->
      apply context
        (D.fromName name)
        (map (srcToDoc App) args)

    Src.TTypeQual _ home name args ->
      apply context
        (D.fromName home <> "." <> D.fromName name)
        (map (srcToDoc App) args)

    Src.TRecord fields ext ->
      record
        (map fieldToDocs fields)
        (fmap (D.fromName . A.toValue) ext)

    Src.TUnit ->
      "()"

    Src.TTuple a b cs ->
      tuple
        (srcToDoc None a)
        (srcToDoc None b)
        (map (srcToDoc None) cs)


fieldToDocs :: (A.Located N.Name, Src.Type) -> (Doc, Doc)
fieldToDocs (A.At _ fieldName, fieldType) =
  ( D.fromName fieldName
  , srcToDoc None fieldType
  )


collectSrcArgs :: Src.Type -> (Src.Type, [Src.Type])
collectSrcArgs tipe =
  case tipe of
    A.At _ (Src.TLambda a result) ->
      let
        (b, cs) = collectSrcArgs result
      in
      (a, b:cs)

    _ ->
      (tipe, [])



-- CANONICAL TYPE TO DOC


canToDoc :: Context -> Can.Type -> Doc
canToDoc context tipe =
  case tipe of
    Can.TLambda arg1 result ->
      let
        (arg2, rest) = collectArgs result
      in
      lambda context
        (canToDoc Func arg1)
        (canToDoc Func arg2)
        (map (canToDoc Func) rest)

    Can.TVar name ->
      D.fromName name

    Can.TType (ModuleName.Canonical _ home) name args ->
      apply context
        (D.fromName home <> "." <> D.fromName name)
        (map (canToDoc App) args)

    Can.TRecord fields ext ->
      record
        (map entryToDocs (Map.toList fields))
        (fmap D.fromName ext)

    Can.TUnit ->
      "()"

    Can.TTuple a b maybeC ->
      tuple
        (canToDoc None a)
        (canToDoc None b)
        (map (canToDoc None) (Maybe.maybeToList maybeC))

    Can.TAlias (ModuleName.Canonical _ home) name args _ ->
      apply context
        (D.fromName home <> "." <> D.fromName name)
        (map (canToDoc App . snd) args)


entryToDocs :: (N.Name, Can.Type) -> (Doc, Doc)
entryToDocs (name, tipe) =
  (D.fromName name, canToDoc None tipe)


collectArgs :: Can.Type -> (Can.Type, [Can.Type])
collectArgs tipe =
  case tipe of
    Can.TLambda a rest ->
      let
        (b, cs) = collectArgs rest
      in
      (a, b:cs)

    _ ->
      (tipe, [])
