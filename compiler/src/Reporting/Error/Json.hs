{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Json
  ( toReport
  )
  where


import qualified Data.Char as Char
import qualified Data.Map as Map

import Json.Decode (Error(..), Problem(..), DecodeExpectation(..), ParseError(..), StringProblem(..))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Render.Code as Code



-- TO REPORT


toReport :: FilePath -> Error x -> Help.Report
toReport path err =
  case err of
    DecodeProblem bytes problem ->
      problemToReport (Code.toSource bytes) problem

    ParseProblem bytes parseError ->
      parseErrorToReport path (Code.toSource bytes) parseError



-- PARSE ERROR TO REPORT


parseErrorToReport :: FilePath -> Code.Source -> ParseError -> Help.Report
parseErrorToReport path source parseError =
  let
    toSnippet title row col pair =
      let pos = A.Position row col in
      Help.jsonReport title (Just path) $
        Code.toSnippet source (A.Region pos pos) Nothing pair
  in
  case parseError of
    Start row col ->
      toSnippet "EXPECTING A VALUE" row col
        (
          D.reflow $ "I was expecting to see a JSON value next:"
        ,
          D.stack
            [ D.fillSep
                ["Try","something","like",D.dullyellow "\"this\"","or"
                ,D.dullyellow "42","to","move","on","to","better","hints!"
                ]
            , D.toSimpleNote $
                "The JSON specification does not allow trailing commas, so you can sometimes\
                \ get this error in arrays that have an extra comma at the end. In that case,\
                \ remove that last comma or add another array entry after it!"
            ]
        )

    ObjectField row col ->
      toSnippet "UNFINISHED OBJECT" row col
        (
          D.reflow $ "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.reflow $ "I was expecting to see a field name next."
            , objectNote
            ]
        )

    ObjectColon row col ->
      toSnippet "EXPECTING COLON" row col
        (
          D.reflow $ "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.reflow $ "I was expecting to see a colon next."
            , objectNote
            ]
        )

    ObjectEnd row col ->
      toSnippet "UNFINISHED OBJECT" row col
        (
          D.reflow $ "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.reflow $
                "I was expecting to see a comma or a closing curly brace next."
            , D.reflow $
                "Is a comma missing on the previous line? Is an array missing a closing square\
                \ bracket? It is often something tricky like that!"
            , objectNote
            ]
        )

    ArrayEnd row col ->
      toSnippet "UNFINISHED ARRAY" row col
        (
          D.reflow $ "I was partway through parsing a JSON array when I got stuck here:"
        ,
          D.stack
            [ D.reflow $ "I was expecting to see a comma or a closing square bracket next."
            , D.reflow $
                "Is a comma missing on the previous line? It is often something like that!"
            ]
        )

    StringProblem stringProblem row col ->
      case stringProblem of
        BadStringEnd ->
          toSnippet "ENDLESS STRING" row col
            (
              D.reflow $
                "I got to the end of the line without seeing the closing double quote:"
            ,
              D.fillSep $
                ["Strings","look","like",D.green "\"this\"","with","double"
                ,"quotes","on","each","end.","Is","the","closing","double"
                ,"quote","missing","in","your","code?"
                ]
            )

        BadStringControlChar ->
          toSnippet "UNEXPECTED CONTROL CHARACTER" row col
            (
              D.reflow $
                "I ran into a control character unexpectedly:"
            ,
              D.reflow $
                "These are characters that represent tabs, backspaces, newlines, and\
                \ a bunch of other invisible characters. They all come before 20 in the\
                \ ASCII range, and they are disallowed by the JSON specificaiton. Maybe\
                \ a copy/paste added one of these invisible characters to your JSON?"
            )

        BadStringEscapeChar ->
          toSnippet "UNKNOWN ESCAPE" row col
            (
              D.reflow $
                "Backslashes always start escaped characters, but I do not recognize this one:"
            ,
              D.stack
                [ D.reflow $
                    "Valid escape characters include:"
                , D.dullyellow $ D.indent 4 $ D.vcat $
                    ["\\\"","\\\\","\\/","\\b","\\f","\\n","\\r","\\t","\\u003D"]
                , D.reflow $
                    "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
                ]
            )

        BadStringEscapeHex ->
          toSnippet "BAD HEX ESCAPE" row col
            (
              D.reflow $
                "This is not a valid hex escape:"
            ,
              D.fillSep $
                ["Valid","hex","escapes","in","JSON","are","between"
                ,D.green "\\u0000","and",D.green "\\uFFFF"
                ,"and","always","have","exactly","four","digits."
                ]
            )

    NoLeadingZeros row col ->
      toSnippet "BAD NUMBER" row col
        (
          D.reflow $ "Numbers cannot start with zeros like this:"
        ,
          D.reflow $ "Try deleting the leading zeros?"
        )

    NoFloats row col ->
      toSnippet "UNEXPECTED NUMBER" row col
        (
          D.reflow $ "I got stuck while trying to parse this number:"
        ,
          D.reflow $
            "I do not accept floating point numbers like 3.1415 right now. That kind\
            \ of JSON value is not needed for any of the uses that Elm has for now."
        )

    BadEnd row col ->
      toSnippet "JSON PROBLEM" row col
        (
          D.reflow $ "I was partway through parsing some JSON when I got stuck here:"
        ,
          D.reflow $
            "I am not really sure what is wrong. This sometimes means there is extra\
            \ stuff after a valid JSON value?"
        )


objectNote :: D.Doc
objectNote =
  D.stack
    [ D.toSimpleNote $ "Here is an example of a valid JSON object for reference:"
    , D.vcat
        [ D.indent 4 $ "{"
        , D.indent 6 $ D.dullyellow "\"name\"" <> ": " <> D.dullyellow "\"Tom\"" <> ","
        , D.indent 6 $ D.dullyellow "\"age\"" <> ": " <> D.dullyellow "42"
        , D.indent 4 $ "}"
        ]
    , D.reflow $
        "Notice that (1) the field names are in double quotes and (2) there is no\
        \ trailing comma after the last entry. Both are strict requirements in JSON!"
    ]



-- PROBLEM TO REPORT


problemToReport :: Code.Source -> Problem x -> Help.Report
problemToReport source problem =
  error "TODO problemToReport" source problem

{-
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


fieldToName :: [Char] -> String
fieldToName field =
  case field of
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
