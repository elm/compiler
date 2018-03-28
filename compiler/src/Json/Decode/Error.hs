{-# LANGUAGE OverloadedStrings #-}
module Json.Decode.Error
  ( Error(..)
  , toDoc
  )
  where


import Prelude hiding (userError)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen ((<+>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Json.Decode.Internals as Json
import qualified Json.Encode as E
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Helpers as H
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report



-- ERROR


data Error e
  = BadJson Syntax.Error
  | BadContent (Json.Error e)



-- TO DOC


toDoc :: String -> Code.Source -> (e -> [H.Doc]) -> Error e -> H.Doc
toDoc rootName source userErrorToDocs err =
  case err of
    BadJson syntaxError ->
      case Syntax.toReport source syntaxError of
        Report.Report _ _ _ doc ->
          doc

    BadContent jsonError ->
      case flatten jsonError of
        [] ->
          H.reflow
            "I am not sure what is wrong with this JSON. Please create an <http://sscce.org>\
            \ and share it at <https://github.com/elm-lang/elm-compiler/issues> so I can\
            \ provide a helpful hint here!"

        [flatError] ->
          flatErrorToDoc rootName [] userErrorToDocs flatError

        flatErrors ->
          let
            toNumberedDoc index flatErr =
              P.dullcyan ("(" <> P.int index <> ")") <+> flatErrorToDoc rootName [] userErrorToDocs flatErr
          in
          H.stack $
            [ H.reflow $
                "I have " ++ show (length flatErrors) ++ " theories on what is going wrong:"
            ]
            ++ zipWith toNumberedDoc [1..] flatErrors



-- FLAT ERROR TO DOC


flatErrorToDoc :: String -> [H.Doc] -> (e -> [H.Doc]) -> FlatError e -> H.Doc
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
          H.fillSep (starter ++ explanation)

        _ ->
          H.stack
            [ H.fillSep $ starter ++ ["The"] ++ actualThing json ++ ["at",accessToDoc rootName accesses,"is","causing","issues."]
            , H.fillSep explanation
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
      H.stack
        [ H.fillSep (starter ++ introduction)
        , H.stack (toBullet [] userErrorToDocs theory : map (toBullet ["OR"] userErrorToDocs) theories)
        , H.reflow "I accept any of these things."
        ]


accessToDoc :: String -> [String] -> H.Doc
accessToDoc rootName accesses =
  P.dullyellow (P.text (rootName ++ concat accesses))


actualThing :: E.Value -> [H.Doc]
actualThing json =
  case json of
    E.Array   _ -> [H.red "array"]
    E.Object  _ -> [H.red "object"]
    E.String  _ -> [H.red "string"]
    E.Boolean b -> [H.red (if b then "true" else "false"),"value"]
    E.Integer n -> ["number",H.red (H.text (show n))]
    E.Number  _ -> [H.red "number"]
    E.Null      -> [H.red "null","value"]


anExpectedThing :: Json.Type -> [H.Doc]
anExpectedThing tipe =
  case tipe of
    Json.TObject -> ["an", P.green "OBJECT" <> "."]
    Json.TArray -> ["an", P.green "ARRAY" <> "."]
    Json.TString -> ["a", P.green "STRING" <> "."]
    Json.TBool -> ["a", P.green "BOOLEAN" <> "."]
    Json.TInt -> ["an", P.green "INT" <> "."]
    Json.TObjectWith field -> ["an",P.green "OBJECT","with","a",P.green ("\"" <> P.text (Text.unpack field) <> "\""),"field."]
    Json.TArrayWith i len ->
      ["a",H.green "longer",P.green "ARRAY" <> "."
      ,"I","need","index",P.text (show i) <> ",","but","this","array"
      ,"only","has",P.text (show len),"elements."
      ]


toBullet :: [H.Doc] -> (e -> [H.Doc]) -> Theory e -> H.Doc
toBullet intro userErrorToDocs theory =
  H.indent 4 $ H.fillSep $ (++) intro $
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
