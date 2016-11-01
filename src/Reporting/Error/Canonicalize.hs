{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Canonicalize where

import Control.Arrow (second)
import qualified Data.Char as Char
import Text.PrettyPrint.ANSI.Leijen (Doc, dullyellow, fillSep, indent, text, vcat)

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Helpers as Help
import qualified Reporting.Region as Region
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report


data Error
    = Var VarError
    | BadRecursion Region.Region Canonical.Def [Canonical.Def]
    | Pattern PatternError
    | Alias AliasError
    | Import ModuleName.Raw ImportError
    | Export String [String]
    | DuplicateExport String
    | Port PortError
    | BadPort String Type.Canonical



-- VARIABLES


data VarError = VarError
    { varKind :: String
    , varName :: String
    , varProblem :: VarProblem
    , varSuggestions :: [String]
    }


data VarProblem
    = Ambiguous
    | UnknownQualifier String String
    | QualifiedUnknown String String
    | ExposedUnknown


variable :: String -> String -> VarProblem -> [String] -> Error
variable kind name problem suggestions =
  Var (VarError kind name problem suggestions)



-- PATTERN


data PatternError
    = PatternArgMismatch Var.Canonical Int Int


argMismatch :: Var.Canonical -> Int -> Int -> Error
argMismatch name expected actual =
  Pattern (PatternArgMismatch name expected actual)



-- IMPORTS


data ImportError
    = ModuleNotFound [ModuleName.Raw]
    | ValueNotFound String [String]


moduleNotFound :: ModuleName.Raw -> [ModuleName.Raw] -> Error
moduleNotFound name possibilities =
  Import name (ModuleNotFound possibilities)


valueNotFound :: ModuleName.Raw -> String -> [String] -> Error
valueNotFound name value possibilities =
  Import name (ValueNotFound value possibilities)



-- ALIASES


data AliasError
    = ArgMismatch Var.Canonical Int Int
    | SelfRecursive String [String] Type.Raw
    | MutuallyRecursive [(Region.Region, String, [String], Type.Raw)]


alias :: Var.Canonical -> Int -> Int -> Error
alias name expected actual =
  Alias (ArgMismatch name expected actual)



-- PORTS


data PortError =
  PortError
    { portName :: String
    , portType :: Type.Canonical
    , portMessage :: Maybe String
    }


port :: String -> Type.Canonical -> Maybe String -> Error
port name tipe maybeMessage =
  Port (PortError name tipe maybeMessage)



-- TO REPORT


namingError :: String -> String -> Report.Report
namingError pre post =
  Report.report "NAMING ERROR" Nothing pre (text post)


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Var (VarError kind name problem suggestions) ->
        let var = kind ++ " `" ++ name ++ "`"
        in
        case problem of
          Ambiguous ->
              namingError
                ("This usage of " ++ var ++ " is ambiguous.")
                (Help.maybeYouWant suggestions)

          UnknownQualifier qualifier localName ->
              namingError
                ("Cannot find " ++ var ++ ".")
                ( "No module called `" ++ qualifier ++ "` has been imported. "
                  ++ Help.maybeYouWant (map (\modul -> modul ++ "." ++ localName) suggestions)
                )

          QualifiedUnknown qualifier localName ->
              namingError
                ("Cannot find " ++ var ++ ".")
                ( "`" ++ qualifier ++ "` does not expose `" ++ localName ++ "`. "
                  ++ Help.maybeYouWant (map (\v -> qualifier ++ "." ++ v) suggestions)
                )

          ExposedUnknown ->
              namingError
                ("Cannot find " ++ var)
                (Help.maybeYouWant suggestions)

    BadRecursion region def defs ->
      case defs of
        [] ->
          badSelfRecursion region (defToString def)

        _ ->
          badMutualRecursion region (defToString def) (map (defToString) defs)


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
                    [ Help.reflowParagraph $
                        "When I expand a recursive type alias, it just keeps getting bigger and bigger.\
                        \ So dealiasing results in an infinitely large type! Try this instead:"
                    , indent 4 $
                        RenderType.decl localizer name tvars [(name, [unsafePromote tipe])]
                    , text $
                        "This is kind of a subtle distinction. I suggested the naive fix, but you can\n"
                        ++ "often do something a bit nicer. So I would recommend reading more at:\n"
                        ++ Help.hintLink "recursive-alias"
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
                    , Help.reflowParagraph $
                        "You need to convert at least one `type alias` into a `type`. This is a kind of\
                        \ subtle distinction, so definitely read up on this before you make a fix: "
                        ++ Help.hintLink "recursive-alias"
                    ]
                )

    Import name importError ->
        let moduleName = ModuleName.toString name
        in
        case importError of
          ModuleNotFound suggestions ->
              namingError
                ("Could not find a module named `" ++ moduleName ++ "`")
                (Help.maybeYouWant (map ModuleName.toString suggestions))

          ValueNotFound value suggestions ->
              namingError
                ("Module `" ++ moduleName ++ "` does not expose `" ++ value ++ "`")
                (Help.maybeYouWant suggestions)

    Export name suggestions ->
        namingError
          ("Could not export `" ++ name ++ "` which is not defined in this module.")
          (Help.maybeYouWant suggestions)

    DuplicateExport name ->
        namingError
          ("You are trying to export `" ++ name ++ "` multiple times!")
          "Remove duplicates until there is only one listed."

    Port (PortError name tipe maybeMessage) ->
      let
        context =
          maybe "" (" the following " ++ ) maybeMessage
      in
        Report.report
          "PORT ERROR"
          Nothing
          ("Port `" ++ name ++ "` is trying to communicate an unsupported type."
          )
          ( Help.stack
              [ text ("The specific unsupported type is" ++ context ++ ":")
              , indent 4 (RenderType.toDoc localizer tipe)
              , text "The types of values that can flow through in and out of Elm include:"
              , indent 4 $ Help.reflowParagraph $
                  "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                  \ Tuples, Json.Values, and concrete records."
              -- TODO add a note about custom decoders and encoders when they exist!
              ]
          )

    BadPort name tipe ->
      Report.report
        "PORT ERROR"
        Nothing
        ("Port `" ++ name ++ "` has an invalid type."
        )
        ( Help.stack
            [ text ("You are saying it should be:")
            , indent 4 (RenderType.toDoc localizer tipe)
            , Help.reflowParagraph $
                "But you need to use the particular format described here:\
                \ <http://guide.elm-lang.org/interop/javascript.html#ports>"
            ]
        )


argMismatchReport :: String -> Var.Canonical -> Int -> Int -> Report.Report
argMismatchReport kind var expected actual =
  let
    numArgs =
      "too "
      ++ (if actual < expected then "few" else "many")
      ++ " arguments"
  in
    Report.report
      (map Char.toUpper numArgs)
      Nothing
      ( kind ++ " " ++ Var.toString var ++ " has " ++ numArgs ++ "."
      )
      (text ("Expecting " ++ show expected ++ ", but got " ++ show actual ++ "."))


extractSuggestions :: Error -> Maybe [String]
extractSuggestions err =
  case err of
    Var (VarError _ _ _ suggestions) ->
        Just suggestions

    BadRecursion _ _ _ ->
        Nothing

    Pattern _ ->
        Nothing

    Alias _ ->
        Nothing

    Import _ importError ->
        case importError of
          ModuleNotFound suggestions ->
              Just (map ModuleName.toString suggestions)

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


unsafePromote :: Type.Raw -> Type.Canonical
unsafePromote (A.A _ rawType) =
  case rawType of
    Type.RLambda arg result ->
        Type.Lambda (unsafePromote arg) (unsafePromote result)

    Type.RVar x ->
        Type.Var x

    Type.RType (Var.Raw name) ->
        Type.Type (Var.local name)

    Type.RApp func args ->
        Type.App (unsafePromote func) (map unsafePromote args)

    Type.RRecord fields ext ->
        Type.Record (map (second unsafePromote) fields) (fmap unsafePromote ext)



-- BAD RECURSION


badSelfRecursion :: Region.Region -> String -> Report.Report
badSelfRecursion region name =
  let
    header =
      Help.functionName name ++ " is defined directly in terms of itself, causing an infinite loop."
  in
    Report.report "BAD RECURSION" (Just region) header $
      Help.stack
        [ badSelfRecursionHelp "Maybe you are trying to mutate a variable?" $
            "Elm does not have mutation, so when I see " ++ Help.functionName name
            ++ " defined in terms of " ++ Help.functionName name
            ++ ", I treat it as a recursive definition. Try giving the new value a new name!"
        , badSelfRecursionHelp "Maybe you DO want a recursive value?" $
            "To define " ++ Help.functionName name ++ " we need to know what " ++ Help.functionName name
            ++ " is, so let’s expand it. Wait, but now we need to know what " ++ Help.functionName name
            ++ " is, so let’s expand it... This will keep going infinitely!"
        , badSelfRecursionHelp "To really learn what is going on and how to fix it, check out:" $
            Help.hintLink "bad-recursion"
        ]


badSelfRecursionHelp :: String -> String -> Doc
badSelfRecursionHelp intro body =
  fillSep $ map (dullyellow . text) (words intro) ++ map text (words body)


badMutualRecursion :: Region.Region -> String -> [String] -> Report.Report
badMutualRecursion region name names =
  let
    header =
      Help.functionName name ++ " is defined in terms of itself in a sneaky way, causing an infinite loop."

    otherDefs =
      if length names == 1 then
        "one other definition"
      else
        show (length names) ++ " other definitions"
  in
    Report.report "BAD RECURSION" (Just region) header $
      Help.stack
        [ Help.reflowParagraph $
            "The following definitions depend directly on each other:"
        , indent 4 $ Help.drawCycle (name : names)
        , Help.reflowParagraph $
            "You seem to have a fairly tricky case, so I very highly recommend reading this: "
            ++ Help.hintLink "bad-recursion"
            ++ " It will help you really understand the problem and how to fix it. Read it!"
        ]


defToString :: Canonical.Def -> String
defToString (Canonical.Def _ pattern _ _) =
  P.toString False pattern
