{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Main
  ( Error(..)
  , toReport
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Helpers as H
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Report as Report



-- ERROR


data Error
  = BadType R.Region Can.Type
  | BadCycle R.Region [N.Name]
  | BadFlags R.Region Can.Type E.InvalidPayload



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    BadType region tipe ->
      Report.Report "BAD MAIN TYPE" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "I cannot handle this type of `main` value:"
          ,
            H.stack
              [ "The type of `main` value I am seeing is:"
              , H.indent 4 $ H.dullyellow $ typeToDoc RT.None tipe
              , H.reflow $
                  "I only know how to handle Html, Svg, and Programs\
                  \ though. Modify `main` to be one of those types of values!"
              ]
          )

    BadCycle region cycleNames ->
      Report.Report "BAD MAIN" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "A `main` definition cannot be defined in terms of itself."
          ,
            H.stack
              [ H.reflow $
                  "It should be a boring value with no recursion. But\
                  \ instead it is involved in this cycle of definitions:"
              , H.indent 4 (H.drawCycle cycleNames)
              ]
          )

    BadFlags region _badType invalidPayload ->
      let
        formatDetails (aBadKindOfThing, butThatIsNoGood) =
          Report.Report "BAD FLAGS" region [] $
            Report.toCodeSnippet source region Nothing
              (
                H.reflow $
                  "Your `main` program wants " ++ aBadKindOfThing ++ " from JavaScript."
              ,
                butThatIsNoGood
              )
      in
      formatDetails $
        case invalidPayload of
          E.ExtendedRecord ->
            (
              "an extended record"
            ,
              H.reflow $
                "But the exact shape of the record must be known at compile time. No type variables!"
            )

          E.Function ->
            (
              "a function"
            ,
              H.reflow $
                "But if I allowed functions from JS, it would be possible to sneak\
                \ side-effects and runtime exceptions into Elm!"
            )

          E.TypeVariable name ->
            (
              "an unspecified type"
            ,
              H.reflow $
                "But type variables like `" ++ N.toString name ++ "` cannot be given as flags.\
                \ I need to know exactly what type of data I am getting, so I can guarantee that\
                \ unexpected data cannot sneak in and crash the Elm program."
            )

          E.UnsupportedType name ->
            (
              "a `" ++ N.toString name ++ "` value"
            ,
              H.stack
                [ H.reflow $ "I cannot handle that. The types that CAN be in flags include:"
                , H.indent 4 $
                    H.reflow $
                      "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                      \ tuples, records, and JSON values."
                , H.reflow $
                    "Since JSON values can flow through, you can use JSON encoders and decoders\
                    \ to allow other types through as well. More advanced users often just do\
                    \ everything with encoders and decoders for more control and better errors."
                ]
            )



-- TYPE TO DOC


typeToDoc :: RT.Context -> Can.Type -> H.Doc
typeToDoc context tipe =
  case tipe of
    Can.TLambda arg1 result ->
      let
        (arg2, rest) = collectArgs result
      in
      RT.lambda context
        (typeToDoc RT.Func arg1)
        (typeToDoc RT.Func arg2)
        (map (typeToDoc RT.Func) rest)

    Can.TVar name ->
      H.nameToDoc name

    Can.TType (ModuleName.Canonical _ home) name args ->
      RT.apply context
        (H.nameToDoc home <> "." <> H.nameToDoc name)
        (map (typeToDoc RT.App) args)

    Can.TRecord fields ext ->
      RT.record
        (map entryToDocs (Map.toList fields))
        (fmap H.nameToDoc ext)

    Can.TUnit ->
      "()"

    Can.TTuple a b maybeC ->
      RT.tuple
        (typeToDoc RT.None a)
        (typeToDoc RT.None b)
        (map (typeToDoc RT.None) (Maybe.maybeToList maybeC))

    Can.TAlias (ModuleName.Canonical _ home) name args _ ->
      RT.apply context
        (H.nameToDoc home <> "." <> H.nameToDoc name)
        (map (typeToDoc RT.App . snd) args)


entryToDocs :: (N.Name, Can.Type) -> (H.Doc, H.Doc)
entryToDocs (name, tipe) =
  (H.nameToDoc name, typeToDoc RT.None tipe)


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
