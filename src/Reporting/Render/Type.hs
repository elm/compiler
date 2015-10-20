module Reporting.Render.Type
  ( Localizer
  , decl
  , aliasDecl
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
  ( Doc, (<+>), cat, colon, comma, dullyellow, equals, hang, hsep, indent
  , lbrace, lparen, rbrace, rparen, sep, text, vcat
  )

import qualified AST.Type as Type
import qualified AST.Variable as Var
import Reporting.Error.Helpers as Help



-- PUBLIC API FOR CREATING DOCS


decl :: Localizer -> String -> [String] -> [(String, [Type.Canonical])] -> Doc
decl localizer name vars tags =
  let
    docTag (tag, args) =
      hang 2 (sep (text tag : map (docType localizer) args))
  in
    hang 4 $ vcat $
      (hsep (map text ("type" : name : vars)))
      :
      zipWith (<+>)
        (equals : repeat (text "|"))
        (map docTag tags)


aliasDecl :: Localizer -> String -> [String] -> Type.Canonical -> Doc
aliasDecl localizer name vars tipe =
  hang 4 $ vcat $
    [ hsep (map text ("type" : name : vars) ++ [equals])
    , docType localizer tipe
    ]


annotation =
  error "TODO"


toDoc :: Localizer -> Type.Canonical -> Doc
toDoc localizer tipe =
  docType localizer tipe


diffToDocs :: Localizer -> Style -> Type.Canonical -> Type.Canonical -> (Doc,Doc)
diffToDocs localizer style leftType rightType =
  case diff localizer style leftType rightType of
    Same doc ->
        (doc, doc)

    Diff leftDoc rightDoc ->
        (leftDoc, rightDoc)



-- LOCALIZE TYPES


type Localizer =
  Map.Map String String


varToDoc :: Localizer -> Var.Canonical -> Doc
varToDoc localizer var =
  let
    name = Var.toString var
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


data Style
    = Elide
    | Full


diff :: Localizer -> Style -> Type.Canonical -> Type.Canonical -> Diff Doc
diff localizer style leftType rightType =
  let
    go = diff localizer style
  in
  case (leftType, rightType) of
    (Type.Lambda _ _, Type.Lambda _ _) ->
        let
          leftParts = Type.collectLambdas leftType
          rightParts = Type.collectLambdas rightType
        in
          if length leftParts /= length rightParts then
              difference
                (docLambda (map (docType localizer) leftParts))
                (docLambda (map (docType localizer) rightParts))

          else
              docLambda <$> sequenceA (zipWith go leftParts rightParts)

    (Type.Var x, Type.Var y) | x == y ->
        pure (text x)

    (Type.Type leftName, Type.Type rightName) | leftName == rightName ->
        pure (varToDoc localizer leftName)

    (Type.App (Type.Type leftName) leftArgs, Type.App (Type.Type rightName) rightArgs) ->
        if leftName /= rightName || length leftArgs /= length rightArgs then
            difference
              (docApp localizer leftName (map (docType localizer) leftArgs))
              (docApp localizer rightName (map (docType localizer) rightArgs))

        else
            docApp localizer leftName
              <$> sequenceA (zipWith go leftArgs rightArgs)

    (Type.App _ _, Type.App _ _) ->
        error "Type applications without concrete names should not get here."

    (Type.Record outerLeftFields outerLeftExt, Type.Record outerRightFields outerRightExt) ->
        let
          (leftFields, leftExt) =
            flattenRecord outerLeftFields outerLeftExt

          (rightFields, rightExt) =
            flattenRecord outerRightFields outerRightExt
        in
          case style of
            Elide ->
                diffRecordElide localizer leftFields leftExt rightFields rightExt

            Full ->
                error "TODO"

    (Type.Aliased leftName leftArgs _, Type.Aliased rightName rightArgs _) | leftName == rightName ->
        docApp localizer leftName
          <$> sequenceA (zipWith go (map snd leftArgs) (map snd rightArgs))

    (Type.Aliased _ args real, _) ->
        go (Type.dealias args real) rightType

    (_, Type.Aliased _ args real) ->
        go leftType (Type.dealias args real)

    (_, _) ->
        difference
          (docType localizer leftType)
          (docType localizer rightType)


difference :: Doc -> Doc -> Diff Doc
difference leftDoc rightDoc =
  Diff (dullyellow leftDoc) (dullyellow rightDoc)



-- RECORD DIFFS


diffRecordElide :: Localizer -> Fields -> Maybe String -> Fields -> Maybe String -> Diff Doc
diffRecordElide localizer leftFields leftExt rightFields rightExt =
  let
    (leftOnly, both, rightOnly) =
      vennDiagram leftFields rightFields
  in
    if Map.null leftOnly && Map.null rightOnly then
        let
          fieldDiffs =
            Map.map (uncurry (diff localizer Elide)) both
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



-- ANALYZE FIELD NAMES FOR SIMILAR NAMES


analyzeFields :: [String] -> [String] -> ( [(Doc, Doc)], [(Doc, Doc)] )
analyzeFields leftOnly rightOnly =
  let
    potentialTypos =
      findPotentialTypos leftOnly rightOnly

    mkField func name =
      ( func (text name), text "…" )

    mkFieldWith counts name =
      mkField (if Set.member name counts then dullyellow else id) name
  in
    case vetTypos potentialTypos of
      Just (leftTypos, rightTypos) ->
        ( map (mkFieldWith leftTypos) leftOnly
        , map (mkFieldWith rightTypos) rightOnly
        )

      Nothing ->
        ( map (mkField dullyellow) leftOnly
        , map (mkField dullyellow) rightOnly
        )


findPotentialTypos :: [String] -> [String] -> [(String, String)]
findPotentialTypos leftOnly rightOnly =
  let
    veryNear leftName =
      map ((,) leftName) (filter ((==1) . Help.distance leftName) rightOnly)
  in
    concatMap veryNear leftOnly


vetTypos :: [(String, String)] -> Maybe (Set.Set String, Set.Set String)
vetTypos potentialTypos =
  let
    tallyNames (ln, rn) (lc, rc) =
      ( Map.insertWith (+) ln 1 lc
      , Map.insertWith (+) rn 1 rc
      )

    (leftCounts, rightCounts) =
      foldr tallyNames (Map.empty, Map.empty) potentialTypos

    allUnique counts =
      Map.foldr (\n unique -> n < 2 && unique) True counts
  in
    if allUnique leftCounts && allUnique rightCounts then
        Just (Map.keysSet leftCounts, Map.keysSet rightCounts)

    else
        Nothing


-- RECORD DIFFS HELPERS


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


docType :: Localizer -> Type.Canonical -> Doc
docType localizer tipe =
  case tipe of
    Type.Lambda _ _ ->
        docLambda (map (docType localizer) (Type.collectLambdas tipe))

    Type.Var x ->
        text x

    Type.Type name ->
        varToDoc localizer name

    Type.App (Type.Type name) args ->
        docApp localizer name (map (docType localizer) args)

    Type.App _ _ ->
        error "type applications should start with a type atom"

    Type.Record outerFields outerExt ->
        let
          (fields, ext) =
            flattenRecordHelp outerFields outerExt
        in
          docRecord Full
            (map (text *** docType localizer) fields)
            (fmap text ext)

    Type.Aliased name args _ ->
        docApp localizer name (map (docType localizer . snd) args)



docLambda :: [Doc] -> Doc
docLambda (arg:rest) =
  sep (arg : map (text "->" <+>) rest)



docApp :: Localizer -> Var.Canonical -> [Doc] -> Doc
docApp localizer name args =
  if Var.isTuple name then
      sep
        [ cat (zipWith (<+>) (lparen : repeat comma) args)
        , rparen
        ]

  else if null args then
      varToDoc localizer name

  else
      hang 4 (sep (varToDoc localizer name : args))



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
            [ text "…" ]

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
          [ lbrace <+> ext
          , indent 4 (cat (zipWith (<+>) (text "|" : repeat comma) fieldDocs))
          , rbrace
          ]

