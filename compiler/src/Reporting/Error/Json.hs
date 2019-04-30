{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Json
  ( Error(..)
  , Problem(..)
  , DecodeExpectation(..)
  , ParseError(..)
  , toReport
  )
  where


import qualified Data.Utf8 as Utf8
import Data.Word (Word16)

import qualified Reporting.Report as Report



-- ERROR


data Error e
  = DecodeProblem (Problem e)
  | ParseProblem ParseError



-- DECODE PROBLEMS


data Problem e
  = Field Utf8.String (Problem e)
  | Index Int (Problem e)
  | OneOf (Problem e) [Problem e]
  | Failure e
  | Expecting DecodeExpectation


data DecodeExpectation
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TObjectWith Utf8.String
  | TArrayPair Int



-- PARSE ERROR


data ParseError
  = PIndex Int ParseError Row Col
  | PField Utf8.String ParseError Row Col
  | ObjectStart Row Col
  | ObjectMore Row Col
  | ObjectEnd Row Col
  | ArrayStart Row Col
  | ArrayMore Row Col
  | ArrayEnd Row Col
  | StringStart Row Col
  | StringEnd Row Col
  | IntStart Row Col
  | NoLeadingZeros Row Col
  | NoFloats Row Col
  | Bool Row Col
  | Null Row Col
  | Value Row Col
  | Colon Row Col
  | EndOfFile Row Col


type Row = Word16
type Col = Word16



-- TO REPORT


toReport :: Error e -> Report.Report
toReport err =
  error "TODO Reporting.Error.Json.toReport" err


{-

-- ERROR


data Error e
  = BadJson Syntax.Error
  | BadContent (Json.Error e)



-- TO DOC


toDoc :: String -> Code.Source -> (e -> [D.Doc]) -> Error e -> D.Doc
toDoc rootName source userErrorToDocs err =
  case err of
    BadJson syntaxError ->
      case Syntax.toReport source syntaxError of
        Report.Report _ _ _ doc ->
          doc

    BadContent jsonError ->
      case flatten jsonError of
        [] ->
          D.reflow
            "I am not sure what is wrong with this JSON. Please create an <http://sscce.org>\
            \ and share it at <https://github.com/elm/compiler/issues> so I can\
            \ provide a helpful hint here!"

        [flatError] ->
          flatErrorToDoc rootName [] userErrorToDocs flatError

        flatErrors ->
          let
            toNumberedDoc index flatErr =
              D.dullcyan ("(" <> D.fromInt index <> ")") <+> flatErrorToDoc rootName [] userErrorToDocs flatErr
          in
          D.stack $
            [ D.reflow $
                "I have " ++ show (length flatErrors) ++ " theories on what is going wrong:"
            ]
            ++ zipWith toNumberedDoc [1..] flatErrors



-- FLAT ERROR TO DOC


flatErrorToDoc :: String -> [D.Doc] -> (e -> [D.Doc]) -> FlatError e -> D.Doc
flatErrorToDoc rootName starter userErrorToDocs (FlatError accesses json theory theories) =
  case theories of
    [] ->
      let
        explanation =
          case theory of
            Failure userError ->
              userErrorToDocs userError

            Expecting tipe ->
              ["I","was","expecting"] ++ anExpectedThing tipe
      in
      case accesses of
        [] ->
          D.fillSep (starter ++ explanation)

        _ ->
          D.stack
            [ D.fillSep $ starter ++ ["The"] ++ actualThing json ++ ["at",accessToDoc rootName accesses,"is","causing","issues."]
            , D.fillSep explanation
            ]

    _:_ ->
      let
        introduction =
          case accesses of
            [] ->
              ["I","am","having","trouble","with","the"]
              ++ actualThing json
              ++ ["here","because:"]

            _ ->
              ["I","am","having","trouble","with","the"]
              ++ actualThing json
              ++ ["at",accessToDoc rootName accesses,"because:"]
      in
      D.stack
        [ D.fillSep (starter ++ introduction)
        , D.stack (toBullet [] userErrorToDocs theory : map (toBullet ["OR"] userErrorToDocs) theories)
        , D.reflow "I accept any of these things."
        ]


accessToDoc :: String -> [String] -> D.Doc
accessToDoc rootName accesses =
  D.dullyellow (D.fromString (rootName ++ concat accesses))


actualThing :: E.Value -> [D.Doc]
actualThing json =
  case json of
    E.Array   _ -> [D.red "array"]
    E.Object  _ -> [D.red "object"]
    E.String  _ -> [D.red "string"]
    E.Boolean b -> [D.red (if b then "true" else "false"),"value"]
    E.Integer n -> ["number",D.red (D.fromInt n)]
    E.Number  _ -> [D.red "number"]
    E.Null      -> [D.red "null","value"]


anExpectedThing :: Json.Type -> [D.Doc]
anExpectedThing tipe =
  case tipe of
    Json.TObject -> ["an", D.green "OBJECT" <> "."]
    Json.TArray -> ["an", D.green "ARRAY" <> "."]
    Json.TString -> ["a", D.green "STRING" <> "."]
    Json.TBool -> ["a", D.green "BOOLEAN" <> "."]
    Json.TInt -> ["an", D.green "INT" <> "."]
    Json.TObjectWith field -> ["an",D.green "OBJECT","with","a",D.green ("\"" <> D.fromString (Text.unpack field) <> "\""),"field."]
    Json.TArrayPair len ->
      ["an",D.green "ARRAY","with",D.green "TWO","entries."
      ,"This","array","has",D.fromInt len, if len == 1 then "element." else "elements."
      ]


toBullet :: [D.Doc] -> (e -> [D.Doc]) -> Theory e -> D.Doc
toBullet intro userErrorToDocs theory =
  D.indent 4 $ D.fillSep $ (++) intro $
    case theory of
      Failure userError ->
        userErrorToDocs userError

      Expecting tipe ->
        ["I","was","expecting"] ++ anExpectedThing tipe



-- TO FLAT ERRORS


data FlatError e =
  FlatError [String] E.Value (Theory e) [Theory e]


data Theory e
  = Failure e
  | Expecting Json.Type


flatten :: Json.Error e -> [FlatError e]
flatten jsonError =
  let
    depth (FlatError accesses _ _ _) =
      length accesses
  in
  List.sortOn depth $ flattenTree (toErrorTree jsonError)


flattenTree :: ErrorTree e -> [FlatError e]
flattenTree (ErrorTree theories subTrees) =
  let
    addAccess access (FlatError accesses json t ts) =
      FlatError (access:accesses) json t ts

    toFlats (name, tree) =
      map (addAccess name) (flattenTree tree)

    subErrors =
      concatMap toFlats (Map.toList subTrees)
  in
  case theories of
    [] ->
      subErrors

    (json, theory) : rest ->
      FlatError [] json theory (map snd rest) : subErrors



-- TO ERROR TREE


data ErrorTree e =
  ErrorTree
    { _theories :: [(E.Value, Theory e)]
    , _subErrors :: Map.Map String (ErrorTree e)
    }


toErrorTree :: Json.Error e -> ErrorTree e
toErrorTree jsonError =
  add jsonError empty


empty :: ErrorTree e
empty =
  ErrorTree [] Map.empty


add :: Json.Error e -> ErrorTree e -> ErrorTree e
add jsonError tree@(ErrorTree theories subErrors) =
  case jsonError of
    Json.Field field subErr ->
      ErrorTree theories (addName (fieldToName field) subErr subErrors)

    Json.Index index subErr ->
      ErrorTree theories (addName (indexToName index) subErr subErrors)

    Json.OneOf errors ->
      foldr add tree errors

    Json.Expecting json tipe ->
      ErrorTree ((json, Expecting tipe) : theories) subErrors

    Json.Failure json err ->
      ErrorTree ((json, Failure err) : theories) subErrors


addName :: String -> Json.Error e -> Map.Map String (ErrorTree e) -> Map.Map String (ErrorTree e)
addName name err subErrors =
  let
    subAdd maybeCrush =
      Just (add err (maybe empty id maybeCrush))
  in
    Map.alter subAdd name subErrors


fieldToName :: Text.Text -> String
fieldToName field =
  case Text.unpack field of
    [] ->
      "['']"

    string@(char : rest) ->
      if Char.isAlpha char && all Char.isAlphaNum rest then
        '.' : string
      else
        "['" ++ string ++ "']"


indexToName :: Int -> String
indexToName index =
  "[" ++ show index ++ "]"

-}
