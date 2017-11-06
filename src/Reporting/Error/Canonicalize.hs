{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize
  ( Error(..)
  , Args(..)
  , CtorType(..)
  , CycleNode(..)
  , InvalidPayload(..)
  , variable
  , VarKind(..)
  , VarProblem(..)
  , toReport
  , extractSuggestions
  )
  where


import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Expression.Canonical as Can
import qualified AST.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ( Doc, (<>), dullyellow, fillSep, green, i2t, indent, reflowParagraph, text )



-- CANONICALIZATION ERRORS


data Error
  = Var VarError
  | Binop N.Name N.Name
  | DuplicateArg N.Name
  | DuplicateDecl N.Name
  | DuplicateType N.Name
  | DuplicateCtor N.Name CtorType CtorType
  | DuplicateBinop N.Name
  | DuplicateField N.Name
  | DuplicateAliasArg N.Name N.Name [N.Name]
  | DuplicateUnionArg N.Name N.Name [N.Name]
  | DuplicateBindingName N.Name
  | EffectNotFound N.Name
  | EffectFunctionNotFound N.Name
  | ExportDuplicate N.Name
  | ExportNotFound VarKind N.Name [N.Name]
  | ExportOpenAlias N.Name
  | ImportNotFound N.Name [N.Name]
  | ImportCtorNotFound N.Name N.Name
  | ImportOpenAlias N.Name
  | ImportExposingNotFound N.Name N.Name [N.Name]
  | PortPayloadInvalid N.Name Type.Canonical InvalidPayload
  | PortTypeInvalid N.Name Type.Canonical
  | RecursiveAlias N.Name [N.Name] Type.Raw [N.Name]
  | RecursiveDecl R.Region N.Name [N.Name]
  | RecursiveLet [CycleNode]
  | Shadowing N.Name
  | TooFew Args N.Name Int Int
  | TooMany Args N.Name Int Int R.Region
  | TupleLargerThanThree R.Region
  | TypeVarsUnboundInUnion N.Name [N.Name] [A.Located N.Name]
  | TypeVarsMessedUpInAlias N.Name [N.Name] [A.Located N.Name] [A.Located N.Name]


data Args
  = UnionArgs
  | AliasArgs
  | PatternArgs


data CtorType
  = UnionCtor N.Name
  | RecordCtor


data CycleNode
  = CycleFunc (A.Located N.Name)
  | CycleValue (A.Located N.Name)
  | CyclePattern Can.Pattern


data InvalidPayload
  = ExtendedRecord
  | Function
  | TypeVariable N.Name
  | UnsupportedType N.Name



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
  = BadOp
  | BadVar
  | BadPattern
  | BadType


toKindInfo :: VarKind -> Text -> ( Doc, Doc, Doc )
toKindInfo kind name =
  case kind of
    BadOp ->
      ( "an", "operator", text $ "(" <> name <> ")" )

    BadVar ->
      ( "a", "value", text name )

    BadPattern ->
      ( "a", "pattern", text name )

    BadType ->
      ( "a", "type", text name )



-- EXTRACT SUGGESTIONS


extractSuggestions :: Error -> Maybe [Text]
extractSuggestions err =
  case err of
    Var (VarError _ _ _ suggestions) ->
        Just suggestions

    Binop _ _ ->
        Nothing

    DuplicateArg _ ->
        Nothing

    DuplicateDecl _ ->
        Nothing

    DuplicateType _ ->
        Nothing

    DuplicateCtor _ _ _ ->
        Nothing

    DuplicateBinop _ ->
        Nothing

    DuplicateField _ ->
        Nothing

    DuplicateAliasArg _ _ _ ->
        Nothing

    DuplicateUnionArg _ _ _ ->
        Nothing

    DuplicateBindingName _ ->
        Nothing

    EffectNotFound _ ->
        Nothing

    EffectFunctionNotFound _ ->
        Nothing

    ExportDuplicate _ ->
        Nothing

    ExportNotFound _ _ suggestions ->
        Just suggestions

    ExportOpenAlias _ ->
        Nothing

    ImportNotFound _ suggestions ->
        Just suggestions

    ImportExposingNotFound _ _ suggestions ->
        Just suggestions

    ImportCtorNotFound _ _ ->
        Nothing

    ImportOpenAlias _ ->
        Nothing

    PortPayloadInvalid _ _ _ ->
        Nothing

    PortTypeInvalid _ _ ->
        Nothing

    RecursiveAlias _ _ _ _ ->
        Nothing

    RecursiveDecl _ _ _ ->
        Nothing

    RecursiveLet _ ->
        Nothing

    Shadowing _ ->
        Nothing

    TooFew _ _ _ _ ->
        Nothing

    TooMany _ _ _ _ _ ->
        Nothing

    TupleLargerThanThree _ ->
        Nothing

    TypeVarsUnboundInUnion _ _ _ ->
        Nothing

    TypeVarsMessedUpInAlias _ _ _ _ ->
        Nothing



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Var varError ->
      varErrorToReport varError

    Binop op1 op2 ->
        Report.report
          "INFIX PROBLEM"
          Nothing
          ("You cannot mix (" <> op1 <> ") and (" <> op2 <> ") without parentheses." )
          (
            Help.stack
              [ reflowParagraph $
                  "I do not know how to group these expressions. Add parentheses for me!"
              ]
          )

    DuplicateArg _ ->
        error "TODO DuplicateArg"

    DuplicateDecl _ ->
        error "TODO DuplicateDecl"

    DuplicateType _ ->
        error "TODO DuplicateType"

    DuplicateCtor _ _ _ ->
        error "TODO DuplicateCtor"

    DuplicateBinop _ ->
        error "TODO DuplicateBinop"

    DuplicateField _ ->
        error "TODO DuplicateField"

    DuplicateAliasArg _ _ _ ->
        error "TODO DuplicateAliasArg"

    DuplicateUnionArg _ _ _ ->
        error "TODO DuplicateUnionArg"

    DuplicateBindingName _ ->
        error "TODO DuplicateBindingName"

    EffectNotFound _ ->
        error "TODO EffectNotFound"

    EffectFunctionNotFound _ ->
        error "TODO EffectFunctionNotFound"

    ExportDuplicate name ->
        Report.report "EXPOSING ERROR" Nothing
          ("You are trying to expose `" <> name <> "` multiple times!")
          "Remove duplicates until there is only one listed."

    ExportNotFound kind rawName suggestions ->
        let (a, thing, name) = toKindInfo kind rawName in
        Report.reportDoc "EXPOSING ERROR" Nothing
          ["You","are","trying","to","expose",a,thing,"named"
          ,name,"but","I","cannot","find","its","definition."
          ]
          (Help.maybeYouWant Nothing suggestions)

    ExportOpenAlias name ->
        error "TODO" name

    ImportNotFound name suggestions ->
        Report.report "IMPORT ERROR" Nothing
          ("Could not find a module named `" <> name <> "`")
          (Help.maybeYouWant Nothing suggestions)

    ImportCtorNotFound ctor tipe ->
        Report.report "IMPORT ERROR" Nothing
          ("This is not the syntax for exposing a `" <> tipe <> "` type constructor:")
          ( Help.stack
              [ reflowParagraph $
                  "Delete that. You can expose the `" <> ctor
                  <> "` type constructors by saying:"
              , indent 4 (green (text (tipe <> "(..)")))
              , reflowParagraph $
                  "This means that every type constructors of `" <> tipe
                  <> "` is exposed. No need to list them separately."
              ]
          )

    ImportOpenAlias name ->
        Report.report "IMPORT ERROR" Nothing
          ("Cannot expose `" <> name <> "` like this:")
          ( reflowParagraph $
              "The (..) syntax exposes type constructors, but a type alias like `"
              <> name <> "` has no constructors! It is just an alternate name for\
              \ an existing type. Remove the (..) part and it should work."
          )
          -- TODO add docs about the difference between types and type aliases

    ImportExposingNotFound name value suggestions ->
        Report.report "IMPORT ERROR" Nothing
          ("Module `" <> name <> "` does not expose `" <> value <> "`")
          (Help.maybeYouWant Nothing suggestions)

    PortPayloadInvalid portName tipe invalidPayload ->
      let
        context =
          case invalidPayload of
            ExtendedRecord ->
              "an extended record"

            Function ->
              "a function"

            TypeVariable name ->
              const "an unknown type" (error "TODO" name)
              -- "Notice the `" <> name <> "` type variable. It must be a concrete type, like Int or Float."

            UnsupportedType name ->
              "a `" <> name <> "` value"
      in
        Report.report
          "PORT ERROR"
          Nothing
          ("Port `" <> portName <> "` is trying to transmit " <> context <> ":"
          )
          (error "TODO" tipe)
          {- Help.stack
              [ text ("The specific unsupported type is" <> context <> ":")
              , indent 4 (RenderType.toDoc localizer tipe)
              , text "The types of values that can flow through in and out of Elm include:"
              , indent 4 $ reflowParagraph $
                  "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                  \ Tuples, Json.Values, and concrete records."
              -- TODO add a note about custom decoders and encoders when they exist!
              ]
          -}

    PortTypeInvalid name tipe ->
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

    RecursiveAlias name args tipe others ->
        aliasRecursionReport localizer name args tipe others

    RecursiveDecl region name names ->
        error "TODO" region name names

    RecursiveLet cycles ->
      case cycles of
        [] ->
          error "TODO"

        [_] ->
          error "TODO" badSelfRecursion

        _ ->
          error "TODO" badMutualRecursion

    Shadowing _ ->
        error "TODO Shadowing"

    TooFew _ _ _ _ ->
        error "TODO TooFew"

    TooMany _ _ _ _ _ ->
        error "TODO TooMany"

    TupleLargerThanThree _ ->
        error "TODO TupleLargerThanThree"

    TypeVarsUnboundInUnion _ _ _ ->
        error "TODO TypeVarsUnboundInUnion"

    TypeVarsMessedUpInAlias _ _ _ _ ->
        error "TODO TypeVarsMessedUpInAlias"



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


_argMismatchReport :: Text -> N.Name -> Int -> Int -> Report.Report
_argMismatchReport kind name expected actual =
  let
    numArgs =
      "too "
      <> (if actual < expected then "few" else "many")
      <> " arguments"
  in
    Report.report
      (Text.map Char.toUpper numArgs)
      Nothing
      ( kind <> " " <> name <> " has " <> numArgs <> "."
      )
      ( text $
          "Expecting " <> i2t expected <> ", but got " <> i2t actual <> "."
      )



-- BAD RECURSION


badSelfRecursion :: R.Region -> Text -> Report.Report
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


badMutualRecursion :: R.Region -> Text -> [Text] -> Report.Report
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



-- BAD ALIAS RECURSION


aliasRecursionReport :: RenderType.Localizer -> N.Name -> [N.Name] -> Type.Raw -> [N.Name] -> Report.Report
aliasRecursionReport localizer name args tipe others =
  case others of
    [] ->
      Report.report "ALIAS PROBLEM" Nothing
        "This type alias is recursive, forming an infinite type!"
        (
          Help.stack
            [ reflowParagraph $
                "When I expand a recursive type alias, it just keeps getting bigger and bigger.\
                \ So dealiasing results in an infinitely large type! Try this instead:"
            , indent 4 $
                RenderType.decl localizer name args [(name, [error "TODO" tipe])]
            , text $
                "This is kind of a subtle distinction. I suggested the naive fix, but you can\n"
                <> "often do something a bit nicer. So I would recommend reading more at:\n"
                <> Help.hintLink "recursive-alias"
            ]
        )

    _ ->
      Report.report "ALIAS PROBLEM" Nothing
        "This type alias is part of a mutually recursive set of type aliases."
        ( Help.stack
            [ text "The following type aliases are mutually recursive:"
            , indent 4 (Help.drawCycle (name:others))
            , reflowParagraph $
                "You need to convert at least one `type alias` into a `type`. This is a kind of\
                \ subtle distinction, so definitely read up on this before you make a fix: "
                <> Help.hintLink "recursive-alias"
            ]
        )
