{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize
  ( Error(..)
  , variable
  , VarKind(..)
  , VarProblem(..)
  , argMismatch
  , moduleNotFound
  , valueNotFound
  , AliasError(..)
  , alias
  , port
  , toReport
  , extractSuggestions
  )
  where

import Control.Arrow ((***))
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Helpers as Help (isOp)
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as Region
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ( Doc, (<>), dullyellow, fillSep, green, i2t, indent, reflowParagraph, text )



-- CANONICALIZATION ERRORS


data Error
    = Var VarError
    | BadRecursion Region.Region Canonical.Def [Canonical.Def]
    | BadInfix Region.Region Int Var.Canonical Decl.Assoc Var.Canonical Decl.Assoc
    | Pattern PatternError
    | Alias AliasError
    | Import ModuleName.Raw ImportError
    | Export Text [Text]
    | DuplicateExport Text
    | Port PortError
    | BadPort Text Type.Canonical



-- VARIABLES


data VarError =
  VarError
    { _kind :: VarKind
    , _name :: Text
    , _problem :: VarProblem
    , _suggestions :: [Text]
    }


data VarProblem
  = Ambiguous
  | UnknownQualifier Text Text
  | QualifiedUnknown Text Text
  | ExposedUnknown


variable :: VarKind -> Text -> VarProblem -> [Text] -> Error
variable kind name problem suggestions =
  Var (VarError kind name problem suggestions)



-- KIND


data VarKind
  = BadValue
  | BadPattern
  | BadType


toKindInfo :: VarKind -> Text -> ( Doc, Doc, Doc )
toKindInfo kind name =
  case kind of
    BadValue ->
      if Help.isOp name then
        ( "an", "operator", text $ "(" <> name <> ")" )
      else
        ( "a", "value", text name )

    BadPattern ->
      ( "a", "pattern", text name )

    BadType ->
      ( "a", "type", text name )



-- PATTERN


data PatternError
    = PatternArgMismatch Var.Canonical Int Int


argMismatch :: Var.Canonical -> Int -> Int -> Error
argMismatch name expected actual =
  Pattern (PatternArgMismatch name expected actual)



-- IMPORTS


data ImportError
    = ModuleNotFound [ModuleName.Raw]
    | ValueNotFound Text [Text]


moduleNotFound :: ModuleName.Raw -> [ModuleName.Raw] -> Error
moduleNotFound name possibilities =
  Import name (ModuleNotFound possibilities)


valueNotFound :: ModuleName.Raw -> Text -> [Text] -> Error
valueNotFound name value possibilities =
  Import name (ValueNotFound value possibilities)



-- ALIASES


data AliasError
    = ArgMismatch Var.Canonical Int Int
    | SelfRecursive Text [Text] Type.Raw
    | MutuallyRecursive [(Region.Region, Text, [Text], Type.Raw)]


alias :: Var.Canonical -> Int -> Int -> Error
alias name expected actual =
  Alias (ArgMismatch name expected actual)



-- PORTS


data PortError =
  PortError
    { _portName :: Text
    , _portType :: Type.Canonical
    , _portMessage :: Maybe Text
    }


port :: Text -> Type.Canonical -> Maybe Text -> Error
port name tipe maybeMessage =
  Port (PortError name tipe maybeMessage)



-- EXTRACT SUGGESTIONS


extractSuggestions :: Error -> Maybe [Text]
extractSuggestions err =
  case err of
    Var (VarError _ _ _ suggestions) ->
        Just suggestions

    BadRecursion _ _ _ ->
        Nothing

    BadInfix _ _ _ _ _ _ ->
        Nothing

    Pattern _ ->
        Nothing

    Alias _ ->
        Nothing

    Import _ importError ->
        case importError of
          ModuleNotFound suggestions ->
              Just (map ModuleName.toText suggestions)

          ValueNotFound _ suggestions ->
              Just suggestions

    Export _ suggestions ->
        Just suggestions

    DuplicateExport _ ->
        Nothing

    Port _ ->
        Nothing

    BadPort _ _ ->
        Nothing



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Var varError ->
      varErrorToReport varError

    BadRecursion region def defs ->
      case defs of
        [] ->
          badSelfRecursion region (defToText def)

        _ ->
          badMutualRecursion region (defToText def) (map (defToText) defs)

    BadInfix region _prec (Var.Canonical _ name1) _assoc1 (Var.Canonical _ name2) _assoc2 ->
        Report.report
          "INFIX PROBLEM"
          (Just region)
          ("You cannot mix (" <> name1 <> ") and (" <> name2 <> ") without parentheses." )
          (
            Help.stack
              [ reflowParagraph $
                  "I do not know how to group these expressions. Add parentheses for me!"
              ]
          )

    Pattern patternError ->
        case patternError of
          PatternArgMismatch var expected actual ->
              argMismatchReport "Pattern" var expected actual

    Alias aliasError ->
        case aliasError of
          ArgMismatch var expected actual ->
              argMismatchReport "Type" var expected actual

          SelfRecursive name tvars tipe ->
              Report.report
                "ALIAS PROBLEM"
                Nothing
                "This type alias is recursive, forming an infinite type!"
                (
                  Help.stack
                    [ reflowParagraph $
                        "When I expand a recursive type alias, it just keeps getting bigger and bigger.\
                        \ So dealiasing results in an infinitely large type! Try this instead:"
                    , indent 4 $
                        RenderType.decl localizer name tvars [(name, [unsafePromote tipe])]
                    , text $
                        "This is kind of a subtle distinction. I suggested the naive fix, but you can\n"
                        <> "often do something a bit nicer. So I would recommend reading more at:\n"
                        <> Help.hintLink "recursive-alias"
                    ]
                )

          MutuallyRecursive aliases ->
              Report.report
                "ALIAS PROBLEM"
                Nothing
                "This type alias is part of a mutually recursive set of type aliases."
                ( Help.stack
                    [ text "The following type aliases are mutually recursive:"
                    , indent 4 (Help.drawCycle (map (\(_, name, _, _) -> name) aliases))
                    , reflowParagraph $
                        "You need to convert at least one `type alias` into a `type`. This is a kind of\
                        \ subtle distinction, so definitely read up on this before you make a fix: "
                        <> Help.hintLink "recursive-alias"
                    ]
                )

    Import name importError ->
        case importError of
          ModuleNotFound suggestions ->
              Report.report "IMPORT ERROR" Nothing
                ("Could not find a module named `" <> ModuleName.toText name <> "`")
                (Help.maybeYouWant Nothing suggestions)

          ValueNotFound value suggestions ->
              Report.report "IMPORT ERROR" Nothing
                ("Module `" <> ModuleName.toText name <> "` does not expose `" <> value <> "`")
                (Help.maybeYouWant Nothing suggestions)

    Export name suggestions ->
        Report.report "EXPOSING ERROR" Nothing
          ("Could not expose `" <> name <> "` which is not defined in this module.")
          (Help.maybeYouWant Nothing suggestions)

    DuplicateExport name ->
        Report.report "EXPOSING ERROR" Nothing
          ("You are trying to expose `" <> name <> "` multiple times!")
          "Remove duplicates until there is only one listed."

    Port (PortError name tipe maybeMessage) ->
      let
        context =
          maybe "" (" the following " <> ) maybeMessage
      in
        Report.report
          "PORT ERROR"
          Nothing
          ("Port `" <> name <> "` is trying to communicate an unsupported type."
          )
          ( Help.stack
              [ text ("The specific unsupported type is" <> context <> ":")
              , indent 4 (RenderType.toDoc localizer tipe)
              , text "The types of values that can flow through in and out of Elm include:"
              , indent 4 $ reflowParagraph $
                  "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                  \ Tuples, Json.Values, and concrete records."
              -- TODO add a note about custom decoders and encoders when they exist!
              ]
          )

    BadPort name tipe ->
      Report.report
        "PORT ERROR"
        Nothing
        ("Port `" <> name <> "` has an invalid type."
        )
        ( Help.stack
            [ text ("You are saying it should be:")
            , indent 4 (RenderType.toDoc localizer tipe)
            , reflowParagraph $
                "But you need to use the particular format described here:\
                \ <http://guide.elm-lang.org/interop/javascript.html#ports>"
            ]
        )



-- VAR ERROR


varErrorToReport :: VarError -> Report.Report
varErrorToReport (VarError kind name problem suggestions) =
  let
    learnMore orMaybe =
      Help.reflowParagraph $
        orMaybe <> " `import` works different than you expect? Learn all about it here: "
        <> Help.hintLink "imports"

    namingError overview maybeStarter specializedSuggestions =
      Report.reportDoc "NAMING ERROR" Nothing overview $
        case Help.maybeYouWant' maybeStarter specializedSuggestions of
          Nothing ->
            learnMore "Maybe"
          Just doc ->
            Help.stack [ doc, learnMore "Or maybe" ]

    specialNamingError specialHint =
      Report.reportDoc "NAMING ERROR" Nothing (cannotFind kind name) (Help.hsep specialHint)
  in
  case problem of
    Ambiguous ->
      namingError (ambiguous kind name) Nothing suggestions

    UnknownQualifier qualifier localName ->
      namingError
        (cannotFind kind name)
        (Just $ text $ "No module called `" <> qualifier <> "` has been imported.")
        (map (\modul -> modul <> "." <> localName) suggestions)

    QualifiedUnknown qualifier localName ->
      namingError
        (cannotFind kind name)
        (Just $ text $ "`" <> qualifier <> "` does not expose `" <> localName <> "`.")
        (map (\v -> qualifier <> "." <> v) suggestions)

    ExposedUnknown ->
      case name of
        "!="  -> specialNamingError (notEqualsHint name)
        "!==" -> specialNamingError (notEqualsHint name)
        "===" -> specialNamingError equalsHint
        "%"   -> specialNamingError modHint
        _     -> namingError (cannotFind kind name) Nothing suggestions


cannotFind :: VarKind -> Text -> [Doc]
cannotFind kind rawName =
  let ( a, thing, name ) = toKindInfo kind rawName in
  [ "Cannot", "find", a, thing, "named", dullyellow name <> ":" ]


ambiguous :: VarKind -> Text -> [Doc]
ambiguous kind rawName =
  let ( _a, thing, name ) = toKindInfo kind rawName in
  [ "This", "usage", "of", "the", dullyellow name, thing, "is", "ambiguous." ]


notEqualsHint :: Text -> [Doc]
notEqualsHint op =
  [ "Looking", "for", "the", "“not", "equal”", "operator?", "The", "traditional"
  , dullyellow $ text $ "(" <> op <> ")"
  , "is", "replaced", "by", green "(/=)", "in", "Elm.", "It", "is", "meant"
  , "to", "look", "like", "the", "“not", "equal”", "sign", "from", "math!", "(≠)"
  ]


equalsHint :: [Doc]
equalsHint =
  [ "A", "special", dullyellow "(===)", "operator", "is", "not", "needed"
  , "in", "Elm.", "We", "use", green "(==)", "for", "everything!"
  ]


modHint :: [Doc]
modHint =
  [ "Rather", "than", "a", dullyellow "(%)", "operator,"
  , "Elm", "has", "a", green "modBy", "function."
  , "Learn", "more", "here:"
  , "<https://package.elm-lang.org/packages/elm-lang/core/latest/Basics#modBy>"
  ]



-- ARG MISMATCH


argMismatchReport :: Text -> Var.Canonical -> Int -> Int -> Report.Report
argMismatchReport kind var expected actual =
  let
    numArgs =
      "too "
      <> (if actual < expected then "few" else "many")
      <> " arguments"
  in
    Report.report
      (Text.map Char.toUpper numArgs)
      Nothing
      ( kind <> " " <> Var.toText var <> " has " <> numArgs <> "."
      )
      ( text $
          "Expecting " <> i2t expected <> ", but got " <> i2t actual <> "."
      )



-- BAD RECURSION


badSelfRecursion :: Region.Region -> Text -> Report.Report
badSelfRecursion region name =
  let
    header =
      Help.functionName name <> " is defined directly in terms of itself, causing an infinite loop."
  in
    Report.report "BAD RECURSION" (Just region) header $
      Help.stack
        [ badSelfRecursionHelp "Maybe you are trying to mutate a variable?" $
            "Elm does not have mutation, so when I see " <> Help.functionName name
            <> " defined in terms of " <> Help.functionName name
            <> ", I treat it as a recursive definition. Try giving the new value a new name!"
        , badSelfRecursionHelp "Maybe you DO want a recursive value?" $
            "To define " <> Help.functionName name <> " we need to know what " <> Help.functionName name
            <> " is, so let’s expand it. Wait, but now we need to know what " <> Help.functionName name
            <> " is, so let’s expand it... This will keep going infinitely!"
        , badSelfRecursionHelp "To really learn what is going on and how to fix it, check out:" $
            Help.hintLink "bad-recursion"
        ]


badSelfRecursionHelp :: Text -> Text -> Doc
badSelfRecursionHelp intro body =
  fillSep $
    map (dullyellow . text) (Text.words intro)
    ++ map text (Text.words body)


badMutualRecursion :: Region.Region -> Text -> [Text] -> Report.Report
badMutualRecursion region name names =
  let
    header =
      Help.functionName name <> " is defined in terms of itself in a sneaky way, causing an infinite loop."
  in
    Report.report "BAD RECURSION" (Just region) header $
      Help.stack
        [ reflowParagraph $
            "The following definitions depend directly on each other:"
        , indent 4 $ Help.drawCycle (name : names)
        , reflowParagraph $
            "You seem to have a fairly tricky case, so I very highly recommend reading this: "
            <> Help.hintLink "bad-recursion"
            <> " It will help you really understand the problem and how to fix it. Read it!"
        ]


defToText :: Canonical.Def -> Text
defToText (Canonical.Def _ pattern _ _) =
  P.toText pattern



-- TYPE COERCION


unsafePromote :: Type.Raw -> Type.Canonical
unsafePromote (A.A _ rawType) =
  case rawType of
    Type.RLambda arg result ->
        Type.Lambda (unsafePromote arg) (unsafePromote result)

    Type.RVar x ->
        Type.Var x

    Type.RType (A.A _ (Var.Raw name)) args ->
        Type.Type (Var.local name) (map unsafePromote args)

    Type.RRecord fields ext ->
        Type.Record (map (A.drop *** unsafePromote) fields) (fmap unsafePromote ext)

