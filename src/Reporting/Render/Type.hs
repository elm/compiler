module Reporting.Render.Type
  ( Localizer
  , decl
  , annotation
  , toDoc
  , diffToDocs
  , Style(..)
  )
  where

import Control.Arrow ((***), first)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<+>), cat, colon, comma, dullyellow, equals, hang, hsep
  , lbrace, lparen, parens, rbrace, rparen, sep, text, vcat
  )

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Error.Helpers as Help



-- PUBLIC API FOR CREATING DOCS


toDoc :: Localizer -> Type.Canonical -> Doc
toDoc localizer tipe =
  docType localizer None tipe


diffToDocs :: Localizer -> Type.Canonical -> Type.Canonical -> (Doc,Doc)
diffToDocs localizer leftType rightType =
  case diff localizer None leftType rightType of
    Same doc ->
        (doc, doc)

    Diff leftDoc rightDoc ->
        (leftDoc, rightDoc)


decl :: Localizer -> String -> [String] -> [(String, [Type.Canonical])] -> Doc
decl localizer name vars tags =
  let
    docTag (tag, args) =
      hang 2 (sep (text tag : map (docType localizer App) args))
  in
    hang 4 $ vcat $
      (hsep (map text ("type" : name : vars)))
      :
      zipWith (<+>)
        (equals : repeat (text "|"))
        (map docTag tags)


annotation :: Localizer -> String -> Type.Canonical -> Doc
annotation localizer name tipe =
  let
    docName =
      if Help.isOp name then parens (text name) else text name
  in
    case Type.collectLambdas tipe of
      parts@(_ : _ : _) ->
          hang 4 $ sep $
            docName
            : zipWith (<+>)
                (colon : repeat (text "->"))
                (map (docType localizer Func) parts)

      _ ->
          hang 4 $ sep $
            [ docName <+> colon
            , docType localizer None tipe
            ]



-- LOCALIZE TYPES


type Localizer =
  Map.Map String String


varToDoc :: Localizer -> Var.Canonical -> Doc
varToDoc localizer var =
  let
    name =
      Var.toString var
  in
    if name == "_Tuple0" then
      text "()"

    else
      text (maybe name id (Map.lookup name localizer))



-- DIFF BUILDER


data Diff a
    = Diff a a
    | Same a


instance Functor Diff where
  fmap func d =
    case d of
      Diff left right ->
          Diff (func left) (func right)

      Same both ->
          Same (func both)


instance Applicative Diff where
  pure a =
    Same a

  (<*>) func arg =
    case (func, arg) of
      (Diff leftFunc rightFunc, Diff leftArg rightArg) ->
          Diff (leftFunc leftArg) (rightFunc rightArg)

      (Diff leftFunc rightFunc, Same arg) ->
          Diff (leftFunc arg) (rightFunc arg)

      (Same func, Diff leftArg rightArg) ->
          Diff (func leftArg) (func rightArg)

      (Same func, Same arg) ->
          Same (func arg)


partitionDiffs :: Map.Map k (Diff a) -> ( [(k, (a,a))], [(k, a)] )
partitionDiffs dict =
  let
    collect key value (diffKeys, sameKeys) =
      case value of
        Diff left right ->
            ( (key, (left, right)) : diffKeys
            , sameKeys
            )

        Same both ->
            ( diffKeys
            , (key, both) : sameKeys
            )
  in
    Map.foldrWithKey collect ([],[]) dict



-- DOC DIFF


data Context
    = None
    | Func
    | App


diff :: Localizer -> Context -> Type.Canonical -> Type.Canonical -> Diff Doc
diff localizer context leftType rightType =
  let
    go = diff localizer
  in
  case (leftType, rightType) of
    (Type.Aliased leftName leftArgs _, Type.Aliased rightName rightArgs _) | leftName == rightName ->
        docAppHelp localizer context leftName
          <$> sequenceA (zipWith (go App) (map snd leftArgs) (map snd rightArgs))

    (Type.Aliased _ args real, _) ->
        go context (Type.dealias args real) rightType

    (_, Type.Aliased _ args real) ->
        go context leftType (Type.dealias args real)

    (Type.Lambda _ _, _) ->
        diffLambda localizer context leftType rightType

    (_, Type.Lambda _ _) ->
        diffLambda localizer context leftType rightType

    (Type.Var x, Type.Var y) | x == y ->
        pure (text x)

    (Type.Type leftName, Type.Type rightName) | leftName == rightName ->
        pure (varToDoc localizer leftName)

    (Type.App (Type.Type leftName) leftArgs, Type.App (Type.Type rightName) rightArgs) ->
        if leftName /= rightName || length leftArgs /= length rightArgs then
            difference
              (docApp localizer context leftName leftArgs)
              (docApp localizer context rightName rightArgs)

        else
            let
              subContext =
                if Var.isTuple leftName then None else App
            in
              docAppHelp localizer context leftName
                <$> sequenceA (zipWith (go subContext) leftArgs rightArgs)

    (Type.App _ _, Type.App _ _) ->
        error "Type applications without concrete names should not get here."

    (Type.Record outerLeftFields outerLeftExt, Type.Record outerRightFields outerRightExt) ->
        let
          (leftFields, leftExt) =
            flattenRecord outerLeftFields outerLeftExt

          (rightFields, rightExt) =
            flattenRecord outerRightFields outerRightExt
        in
          diffRecord localizer leftFields leftExt rightFields rightExt

    (_, _) ->
        difference
          (docType localizer context leftType)
          (docType localizer context rightType)


difference :: Doc -> Doc -> Diff Doc
difference leftDoc rightDoc =
  Diff (dullyellow leftDoc) (dullyellow rightDoc)



-- FUNCTION DIFFS


diffLambda :: Localizer -> Context -> Type.Canonical -> Type.Canonical -> Diff Doc
diffLambda localizer context leftType rightType =
  let
    leftArgs = reverse $ Type.collectLambdas leftType
    rightArgs = reverse $ Type.collectLambdas rightType

    extraToDoc types =
      map (dullyellow . docType localizer Func) types

    extraLefts = reverse $ extraToDoc $ drop (length rightArgs) leftArgs
    extraRights = reverse $ extraToDoc $ drop (length leftArgs) rightArgs

    alignedArgDiff =
      reverse <$> sequenceA (zipWith (diff localizer Func) leftArgs rightArgs)
  in
    docLambda context <$>
      case (extraLefts, extraRights, alignedArgDiff) of
        ([], [], _) ->
          alignedArgDiff

        (_, _, Same docs) ->
          Diff (extraLefts ++ docs) (extraRights ++ docs)

        (_, _, Diff lefts rights) ->
          Diff (extraLefts ++ lefts) (extraRights ++ rights)



-- RECORD DIFFS


diffRecord :: Localizer -> Fields -> Maybe String -> Fields -> Maybe String -> Diff Doc
diffRecord localizer leftFields leftExt rightFields rightExt =
  let
    (leftOnly, both, rightOnly) =
      vennDiagram leftFields rightFields
  in
    if Map.null leftOnly && Map.null rightOnly then
        let
          fieldDiffs =
            Map.map (uncurry (diff localizer None)) both
        in
          case partitionDiffs fieldDiffs of
            ([], sames) ->
                let
                  mkRecord =
                    docRecord Full (map (first text) sames)
                in
                  if leftExt == rightExt then
                      Same (mkRecord (fmap text leftExt))

                  else
                      Diff
                        (mkRecord (fmap (dullyellow . text) leftExt))
                        (mkRecord (fmap (dullyellow . text) rightExt))

            (diffs, sames) ->
                let
                  (lefts, rights) = unzipDiffs diffs
                  style = if null sames then Full else Elide
                in
                  Diff
                    (docRecord style lefts (fmap text leftExt))
                    (docRecord style rights (fmap text rightExt))

    else
        let
          (lefts, rights) =
            analyzeFields (Map.keys leftOnly) (Map.keys rightOnly)

          style =
            if Map.null both then Full else Elide
        in
          Diff
            (docRecord style lefts (fmap text leftExt))
            (docRecord style rights (fmap text rightExt))


unzipDiffs :: [(String, (Doc, Doc))] -> ( [(Doc, Doc)], [(Doc, Doc)] )
unzipDiffs diffPairs =
  let
    unzipHelp (name, (left, right)) =
      (,) (text name, left) (text name, right)
  in
    unzip (map unzipHelp diffPairs)



-- RECORD DIFFS HELPERS


analyzeFields :: [String] -> [String] -> ( [(Doc, Doc)], [(Doc, Doc)] )
analyzeFields leftOnly rightOnly =
  let
    typoPairs =
      Help.findTypoPairs leftOnly rightOnly

    mkField func name =
      ( func (text name), text "..." )

    mkFieldWith counts name =
      mkField (if Set.member name counts then dullyellow else id) name
  in
    case Help.vetTypos typoPairs of
      Just (leftTypos, rightTypos) ->
        ( map (mkFieldWith leftTypos) leftOnly
        , map (mkFieldWith rightTypos) rightOnly
        )

      Nothing ->
        ( map (mkField dullyellow) leftOnly
        , map (mkField dullyellow) rightOnly
        )


type Fields =
  Map.Map String Type.Canonical


type DoubleFields =
  Map.Map String (Type.Canonical,Type.Canonical)


vennDiagram :: Fields -> Fields -> (Fields, DoubleFields, Fields)
vennDiagram leftFields rightFields =
  ( Map.difference leftFields rightFields
  , Map.intersectionWith (,) leftFields rightFields
  , Map.difference rightFields leftFields
  )


flattenRecord
    :: [(String, Type.Canonical)]
    -> Maybe Type.Canonical
    -> (Fields, Maybe String)
flattenRecord fields ext =
  first Map.fromList (flattenRecordHelp fields ext)


flattenRecordHelp
    :: [(String, Type.Canonical)]
    -> Maybe Type.Canonical
    -> ([(String, Type.Canonical)], Maybe String)
flattenRecordHelp fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (Type.Var x) ->
        (fields, Just x)

    Just (Type.Record subFields subExt) ->
        flattenRecordHelp (fields ++ subFields) subExt

    Just (Type.Aliased _ args tipe) ->
        flattenRecordHelp fields (Just (Type.dealias args tipe))

    _ ->
        error "Trying to flatten ill-formed record."



-- TYPES TO DOCS


docType :: Localizer -> Context -> Type.Canonical -> Doc
docType localizer context tipe =
  case tipe of
    Type.Lambda _ _ ->
        docLambda context (map (docType localizer Func) (Type.collectLambdas tipe))

    Type.Var x ->
        text x

    Type.Type name ->
        varToDoc localizer name

    Type.App (Type.Type name) args ->
        docApp localizer context name args

    Type.App _ _ ->
        error "type applications should start with a type atom"

    Type.Record outerFields outerExt ->
        let
          (fields, ext) =
            flattenRecordHelp outerFields outerExt
        in
          docRecord Full
            (map (text *** docType localizer None) fields)
            (fmap text ext)

    Type.Aliased name args _ ->
        docApp localizer context name (map snd args)



docLambda :: Context -> [Doc] -> Doc
docLambda context docs =
  case docs of
    [] ->
        error "cannot call docLambda with an empty list"

    arg:rest ->
        case context of
          None ->
              sep (arg : map (text "->" <+>) rest)

          _ ->
              cat
                [ lparen
                , sep (arg : map (text "->" <+>) rest)
                , rparen
                ]



docApp :: Localizer -> Context -> Var.Canonical -> [Type.Canonical] -> Doc
docApp localizer context name args =
  let
    argContext =
      if Var.isTuple name then None else App
  in
    docAppHelp localizer context name (map (docType localizer argContext) args)


docAppHelp :: Localizer -> Context -> Var.Canonical -> [Doc] -> Doc
docAppHelp localizer context name args =
  if Var.isTuple name then
      sep
        [ cat (zipWith (<+>) (lparen : repeat comma) args)
        , rparen
        ]

  else if null args then
      varToDoc localizer name

  else
      case context of
        App ->
            cat
              [ lparen
              , hang 4 (sep (varToDoc localizer name : args))
              , rparen
              ]

        _ ->
            hang 4 (sep (varToDoc localizer name : args))



data Style
    = Elide
    | Full


docRecord :: Style -> [(Doc,Doc)] -> Maybe Doc -> Doc
docRecord style fields maybeExt =
  let
    docField (name, tipe) =
      hang 4 (sep [ name <+> colon, tipe ])

    elision =
      case style of
        Full ->
            []

        Elide ->
            [ text "..." ]

    fieldDocs =
      elision ++ map docField fields
  in
  case (fieldDocs, maybeExt) of
    ([], Nothing) ->
        text "{}"

    (_, Nothing) ->
        sep
          [ cat (zipWith (<+>) (lbrace : repeat comma) fieldDocs)
          , rbrace
          ]

    (_, Just ext) ->
        sep
          [ hang 4 $ sep $
              [ lbrace <+> ext
              , cat (zipWith (<+>) (text "|" : repeat comma) fieldDocs)
              ]
          , rbrace
          ]

