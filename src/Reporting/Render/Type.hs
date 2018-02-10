{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Type
  ( Context(..)
  , lambda
  , apply
  , tuple
  , record
  , recordSnippet
  )
  where


import qualified Reporting.Helpers as H
import Reporting.Helpers ( Doc, (<+>) )



-- TO DOC


data Context
  = None
  | Func
  | App


lambda :: Context -> Doc -> Doc -> [Doc] -> Doc
lambda context arg1 arg2 args =
  let
    lambdaDoc =
      H.sep (arg1 : map ("->" <+>) (arg2:args))
  in
  case context of
    None -> lambdaDoc
    Func -> H.cat [ "(", lambdaDoc, ")" ]
    App  -> H.cat [ "(", lambdaDoc, ")" ]


apply :: Context -> Doc -> [Doc] -> Doc
apply context name args =
  case args of
    [] ->
      name

    _:_ ->
      let
        applyDoc =
          H.hang 4 (H.sep (name : args))
      in
      case context of
        App  -> H.cat [ "(", applyDoc, ")" ]
        Func -> applyDoc
        None -> applyDoc


tuple :: Doc -> Doc -> [Doc] -> Doc
tuple a b cs =
  let
    entries =
      zipWith (<+>) ("(" : repeat ",") (a:b:cs)
  in
  H.sep [ H.cat entries, ")" ]


record :: [(Doc, Doc)] -> Maybe Doc -> Doc
record entries maybeExt =
  case (map entryToDoc entries, maybeExt) of
    ([], Nothing) ->
        "{}"

    (fields, Nothing) ->
        H.sep
          [ H.cat (zipWith (<+>) ("{" : repeat ",") fields)
          , "}"
          ]

    (fields, Just ext) ->
        H.sep
          [ H.hang 4 $ H.sep $
              [ "{" <+> ext
              , H.cat (zipWith (<+>) ("|" : repeat ",") fields)
              ]
          , "}"
          ]


entryToDoc :: (Doc, Doc) -> Doc
entryToDoc (fieldName, fieldType) =
  H.hang 4 (H.sep [ fieldName <+> ":", fieldType ])


recordSnippet :: (Doc, Doc) -> [(Doc, Doc)] -> Doc
recordSnippet entry entries =
  let
    field  = "{" <+> entryToDoc entry
    fields = zipWith (<+>) (repeat ",") (map entryToDoc entries ++ ["..."])
  in
  H.sep [ H.cat (field:fields), "}" ]
