{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Error
  ( Type(..)
  , Extension(..)
  , iteratedDealias
  , Localizer
  , toDoc
  , toDiffDocs
  , Problem(..)
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))

import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Elm.Name as N
import qualified Reporting.Helpers as H
import qualified Reporting.Render.Type as RT



-- ERROR TYPES


data Type
  = Lambda Type Type [Type]
  | Infinite
  | Error
  | FlexVar N.Name
  | FlexSuper N.Name
  | RigidVar N.Name
  | RigidSuper N.Name
  | Type ModuleName.Canonical N.Name [Type]
  | Record (Map.Map N.Name Type) Extension
  | Unit
  | Tuple Type Type (Maybe Type)
  | Alias ModuleName.Canonical N.Name [(N.Name, Type)] Type


data Extension
  = Closed
  | FlexOpen N.Name
  | RigidOpen N.Name


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    Alias _ _ _ realType ->
      iteratedDealias realType

    _ ->
      tipe



-- TO DOC


type Localizer = Map.Map (ModuleName.Canonical, N.Name) String


nameToDoc :: Localizer -> ModuleName.Canonical -> N.Name -> H.Doc
nameToDoc dict home@(ModuleName.Canonical _ moduleName) name =
  case Map.lookup (home, name) dict of
    Nothing ->
      H.nameToDoc moduleName <> "." <> H.nameToDoc name

    Just string ->
      H.text string


toDoc :: Localizer -> RT.Context -> Type -> H.Doc
toDoc dict ctx tipe =
  case tipe of
    Lambda a b cs ->
      RT.lambda ctx
        (toDoc dict RT.Func a)
        (toDoc dict RT.Func b)
        (map (toDoc dict RT.Func) cs)

    Infinite ->
      "∞"

    Error ->
      "?"

    FlexVar name ->
      H.nameToDoc name

    FlexSuper name ->
      H.nameToDoc name

    RigidVar name ->
      H.nameToDoc name

    RigidSuper name ->
      H.nameToDoc name

    Type home name args ->
      RT.apply ctx
        (nameToDoc dict home name)
        (map (toDoc dict RT.App) args)

    Record fields ext ->
      let
        entryToDocs (fieldName, fieldType) =
          ( H.nameToDoc fieldName, toDoc dict RT.None fieldType )

        fieldDocs =
          map entryToDocs (Map.toList fields)
      in
      RT.record fieldDocs $
        case ext of
          Closed -> Nothing
          FlexOpen x -> Just (H.nameToDoc x)
          RigidOpen x -> Just (H.nameToDoc x)


    Unit ->
      "()"

    Tuple a b maybeC ->
      RT.tuple
        (toDoc dict RT.None a)
        (toDoc dict RT.None b)
        (map (toDoc dict RT.None) (Maybe.maybeToList maybeC))

    Alias home name args _ ->
      RT.apply ctx
        (nameToDoc dict home name)
        (map (toDoc dict RT.App . snd) args)



-- DIFF


toDiffDocs :: Localizer -> Type -> Type -> (H.Doc, H.Doc, [Problem])
toDiffDocs dict a b =
  case diff dict RT.None a b of
    Similar aDoc bDoc ->
      (aDoc, bDoc, [])

    Different aDoc bDoc problems ->
      (aDoc, bDoc, Bag.toList problems)


data Diff a
  = Similar a a
  | Different a a (Bag.Bag Problem)


instance Functor Diff where
  fmap func d =
    case d of
      Similar   a b    -> Similar   (func a) (func b)
      Different a b ps -> Different (func a) (func b) ps


instance Applicative Diff where
  pure a =
    Similar a a

  (<*>) dFunc dArg =
    case dFunc of
      Similar aFunc bFunc ->
        case dArg of
          Similar   a b     -> Similar   (aFunc a) (bFunc b)
          Different a b ps  -> Different (aFunc a) (bFunc b) ps

      Different aFunc bFunc ps ->
        case dArg of
          Similar   a b     -> Different (aFunc a) (bFunc b) ps
          Different a b ps2 -> Different (aFunc a) (bFunc b) (Bag.append ps ps2)



-- PROBLEMS


data Problem
  = FieldMismatch [N.Name] [N.Name]
  | IntFloat
  | MissingArgs Int
  | ReturnMismatch Int Int
  | TupleMismatch
  | BadFlexSuper N.Name Type
  | BadRigidVar N.Name Type
  | BadRigidSuper N.Name Type



-- COMPUTE DIFF


diff :: Localizer -> RT.Context -> Type -> Type -> Diff H.Doc
diff dict ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (FlexVar    x, FlexVar    y) | x == y -> pure (H.nameToDoc x)
    (FlexSuper  x, FlexSuper  y) | x == y -> pure (H.nameToDoc x)
    (RigidVar   x, RigidVar   y) | x == y -> pure (H.nameToDoc x)
    (RigidSuper x, RigidSuper y) | x == y -> pure (H.nameToDoc x)

    (Infinite, Infinite) -> pure "∞"
    (Error, Error) -> pure "?"
    (Unit, Unit) -> pure "()"

    (Tuple a b maybeC, Tuple x y maybeZ) ->
        RT.tuple
          <$> diff dict RT.None a x
          <*> diff dict RT.None b y
          <*>
            case (maybeC, maybeZ) of
              (Nothing, Nothing) -> pure []
              (Just c , Nothing) -> Different [H.dullyellow (toDoc dict RT.None c)] [] (Bag.one TupleMismatch)
              (Nothing, Just z ) -> Different [] [H.dullyellow (toDoc dict RT.None z)] (Bag.one TupleMismatch)
              (Just c , Just z ) -> (:[]) <$> diff dict RT.None c z

    (Record fields1 ext1, Record fields2 ext2) -> diffRecord dict fields1 ext1 fields2 ext2

    (Type home1 name1 args1, Type home2 name2 args2) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (nameToDoc dict home1 name1) <$>
        sequenceA (zipWith (diff dict RT.App) args1 args2)

    (Alias home1 name1 args1 _, Alias home2 name2 args2 _) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (nameToDoc dict home1 name1) <$>
        sequenceA (zipWith (diff dict RT.App) (map snd args1) (map snd args2))

    (Alias _ _ _ alias1, other) -> diff dict ctx alias1 other
    (other, Alias _ _ _ alias2) -> diff dict ctx other alias2

    (Lambda a b cs, Lambda x y zs) -> diffLambda dict ctx (a:b:cs) (x:y:zs)
    (Lambda a b cs, result       ) -> diffLambda dict ctx (a:b:cs) [result]
    (result       , Lambda x y zs) -> diffLambda dict ctx [result] (x:y:zs)

    (FlexVar x, other) -> Similar (H.nameToDoc x) (toDoc dict ctx other)
    (other, FlexVar x) -> Similar (toDoc dict ctx other) (H.nameToDoc x)

    pair ->
      let
        doc1 = H.dullyellow (toDoc dict ctx tipe1)
        doc2 = H.dullyellow (toDoc dict ctx tipe2)
      in
      Different doc1 doc2 $
        case pair of
          (RigidVar   x, other) -> Bag.one $ BadRigidVar x other
          (FlexSuper  x, other) -> Bag.one $ BadFlexSuper x other
          (RigidSuper x, other) -> Bag.one $ BadRigidSuper x other
          (other, RigidVar   x) -> Bag.one $ BadRigidVar x other
          (other, FlexSuper  x) -> Bag.one $ BadFlexSuper x other
          (other, RigidSuper x) -> Bag.one $ BadRigidSuper x other

          (Type home1 name1 [], Type home2 name2 []) | isIntFloat home1 name1 home2 name2 -> Bag.one IntFloat

          (_, _) -> Bag.empty


isIntFloat :: ModuleName.Canonical -> N.Name -> ModuleName.Canonical -> N.Name -> Bool
isIntFloat home1 name1 home2 name2 =
  home1 == ModuleName.basics
  &&
  home2 == ModuleName.basics
  &&
  (
    (name1 == N.int && name2 == N.float)
    ||
    (name1 == N.float && name2 == N.int)
  )



-- DIFF LAMBDAS


--
-- INVARIANT: notNull types1 && notNull types2
--
diffLambda :: Localizer -> RT.Context -> [Type] -> [Type] -> Diff H.Doc
diffLambda dict ctx types1 types2 =
  let
    (result1:revArgs1) = reverse types1
    (result2:revArgs2) = reverse types2

    numArgs1 = length revArgs1
    numArgs2 = length revArgs2

    resultDiff = diff dict RT.Func result1 result2
  in
  case resultDiff of
    Similar resultDoc1 resultDoc2 ->
      if numArgs1 == numArgs2 then
        (\resultDoc revArgsDoc ->
            let (x:y:zs) = reverse (resultDoc:revArgsDoc) in
            RT.lambda ctx x y zs
        )
          <$> resultDiff
          <*> sequenceA (zipWith (diff dict RT.Func) revArgs1 revArgs2)

      else if numArgs1 < numArgs2 then
        diffArgMismatch dict ctx revArgs1 resultDoc1 revArgs2 resultDoc2

      else
        diffArgMismatch dict ctx revArgs2 resultDoc2 revArgs1 resultDoc1

    Different resultDoc1 resultDoc2 problems ->
      let
        (x1:y1:zs1) = reverse (resultDoc1 : map (toDoc dict RT.Func) revArgs1)
        (x2:y2:zs2) = reverse (resultDoc2 : map (toDoc dict RT.Func) revArgs2)
      in
      Different
        (RT.lambda ctx x1 y1 zs1)
        (RT.lambda ctx x2 y2 zs2)
        (Bag.append (Bag.one (ReturnMismatch numArgs1 numArgs2)) problems)


--
-- INVARIANT: notNull shortRevArgs && notNull longRevArgs
--
diffArgMismatch :: Localizer -> RT.Context -> [Type] -> H.Doc -> [Type] -> H.Doc -> Diff H.Doc
diffArgMismatch dict ctx shortRevArgs shortResult longRevArgs longResult =
  case toGreedyMatch dict shortRevArgs longRevArgs of
    Just (GreedyMatch shortRevArgDocs longRevArgDocs) ->
      let
        (a:b:cs) = reverse (shortResult:shortRevArgDocs)
        (x:y:zs) = reverse (longResult:longRevArgDocs)
      in
      Different
        (RT.lambda ctx a b cs)
        (RT.lambda ctx x y zs)
        (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))

    Nothing ->
      case toGreedyMatch dict (reverse shortRevArgs) (reverse longRevArgs) of
        Just (GreedyMatch shortArgDocs longArgDocs) ->
          let
            (a:b:cs) = shortArgDocs ++ [shortResult]
            (x:y:zs) = longArgDocs  ++ [longResult ]
          in
          Different
            (RT.lambda ctx a b cs)
            (RT.lambda ctx x y zs)
            (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))

        Nothing ->
          let
            toYellowDoc tipe =
              H.dullyellow (toDoc dict RT.Func tipe)

            (a:b:cs) = reverse (shortResult : map toYellowDoc shortRevArgs)
            (x:y:zs) = reverse (longResult  : map toYellowDoc longRevArgs )
          in
          Different
            (RT.lambda ctx a b cs)
            (RT.lambda ctx x y zs)
            (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))



-- GREEDY ARG MATCHER


data GreedyMatch =
  GreedyMatch [H.Doc] [H.Doc]


--
-- INVARIANT: length shorterArgs < length longerArgs
--
toGreedyMatch :: Localizer -> [Type] -> [Type] -> Maybe GreedyMatch
toGreedyMatch dict shorterArgs longerArgs =
  toGreedyMatchHelp dict shorterArgs longerArgs (GreedyMatch [] [])


toGreedyMatchHelp :: Localizer -> [Type] -> [Type] -> GreedyMatch -> Maybe GreedyMatch
toGreedyMatchHelp dict shorterArgs longerArgs match@(GreedyMatch shorterDocs longerDocs) =
  let
    toYellowDoc tipe =
      H.dullyellow (toDoc dict RT.Func tipe)
  in
  case (shorterArgs, longerArgs) of
    (x:xs, y:ys) ->
      case diff dict RT.Func x y of
        Similar a b ->
          toGreedyMatchHelp dict xs ys $
            GreedyMatch (a:shorterDocs) (b:longerDocs)

        Different _ _ _ ->
          toGreedyMatchHelp dict shorterArgs ys $
            GreedyMatch shorterDocs (toYellowDoc y : longerDocs)

    ([], []) ->
      Just match

    ([], _:_) ->
      Just (GreedyMatch shorterDocs (map toYellowDoc longerArgs))

    (_:_, []) ->
      Nothing



-- RECORD DIFFS


diffRecord :: Localizer -> Map.Map N.Name Type -> Extension -> Map.Map N.Name Type -> Extension -> Diff H.Doc
diffRecord dict fields1 ext1 fields2 ext2 =
  let
    only1 = Map.keys (Map.difference fields1 fields2)
    only2 = Map.keys (Map.difference fields2 fields1)
  in
  if null only1 && null only2 then
    case findBadOverlaps dict fields1 fields2 of
      [] ->
        case diffExt ext1 ext2 of
          Similar _ _ ->
            Similar (toBoringRecord fields1 ext1) (toBoringRecord fields2 ext2)

          Different dExt1 dExt2 problems ->
            Different (RT.record [] dExt1) (RT.record [] dExt2) problems

      overlaps@(bad:bads) ->
        if length overlaps < Map.size fields1 then
          Different
            (RT.recordSnippet (toOverlapDoc _doc1 bad) (map (toOverlapDoc _doc1) bads))
            (RT.recordSnippet (toOverlapDoc _doc2 bad) (map (toOverlapDoc _doc2) bads))
            (foldr Bag.append Bag.empty (map _problems overlaps))
        else
          Different
            (toOverlapRecord _doc1 bad bads ext1)
            (toOverlapRecord _doc2 bad bads ext2)
            (foldr Bag.append Bag.empty (map _problems overlaps))

  else
    Different
      (toMissingDoc fields1 only1 ext1)
      (toMissingDoc fields2 only2 ext2)
      (Bag.one (FieldMismatch only1 only2))


toOverlapDoc :: (BadOverlap -> H.Doc) -> BadOverlap -> (H.Doc, H.Doc)
toOverlapDoc getDoc overlap@(BadOverlap field _ _ _) =
  (H.nameToDoc field, getDoc overlap)


toOverlapRecord :: (BadOverlap -> H.Doc) -> BadOverlap -> [BadOverlap] -> Extension -> H.Doc
toOverlapRecord getDoc bad bads ext =
  let go = toOverlapDoc getDoc in
  case ext of
    Closed      -> RT.record (map go (bad:bads)) Nothing
    FlexOpen  _ -> RT.recordSnippet (go bad) (map go bads)
    RigidOpen _ -> RT.recordSnippet (go bad) (map go bads)


toMissingDoc :: Map.Map N.Name t -> [N.Name] -> Extension -> H.Doc
toMissingDoc allFields uniqueFields ext =
  case map emphasizeFieldName uniqueFields of
    [] ->
      toBoringRecord allFields ext

    doc:docs ->
      if length uniqueFields < Map.size allFields then
        RT.recordSnippet doc docs
      else
        case ext of
          Closed      -> RT.record (doc:docs) Nothing
          FlexOpen _  -> RT.recordSnippet doc docs
          RigidOpen _ -> RT.recordSnippet doc docs


emphasizeFieldName :: N.Name -> (H.Doc, H.Doc)
emphasizeFieldName field =
  ( H.dullyellow (H.nameToDoc field), "..." )


toBoringRecord :: Map.Map N.Name t -> Extension -> H.Doc
toBoringRecord fields ext =
  case ext of
    Closed      -> if Map.null fields then "{}" else "{ ... }"
    FlexOpen _  -> "{ ... }"
    RigidOpen _ -> "{ ... }"



-- DIFF RECORD EXTENSION


diffExt :: Extension -> Extension -> Diff (Maybe H.Doc)
diffExt ext1 ext2 =
  let
    normal = Just . H.nameToDoc
    yellow = Just . H.dullyellow . H.nameToDoc

    different x y = Different x y Bag.empty
  in
  case ext1 of
    Closed ->
      case ext2 of
        Closed      -> Similar Nothing Nothing
        FlexOpen  y -> different Nothing (yellow y)
        RigidOpen y -> different Nothing (yellow y)

    FlexOpen x ->
      case ext2 of
        Closed      -> different (yellow x) Nothing
        FlexOpen  y -> Similar (normal x) (normal y)
        RigidOpen y -> Similar (normal x) (normal y)

    RigidOpen x ->
      case ext2 of
        Closed      -> different (yellow x) Nothing
        FlexOpen  y -> Similar (normal x) (normal y)
        RigidOpen y ->
          if x == y then
            Similar (normal x) (normal y)
          else
            Different (yellow x) (yellow y) (Bag.one (BadRigidVar x (RigidVar y)))



-- BAD OVERLAP


data BadOverlap =
  BadOverlap
    { _field :: N.Name
    , _doc1 :: H.Doc
    , _doc2 :: H.Doc
    , _problems :: Bag.Bag Problem
    }


findBadOverlaps :: Localizer -> Map.Map N.Name Type -> Map.Map N.Name Type -> [BadOverlap]
findBadOverlaps dict fields1 fields2 =
  findBadOverlapsHelp [] $ Map.toList $
    Map.intersectionWith (diff dict RT.None) fields1 fields2


findBadOverlapsHelp :: [BadOverlap] -> [(N.Name, Diff H.Doc)] -> [BadOverlap]
findBadOverlapsHelp badOverlaps fieldPairs =
  case fieldPairs of
    [] ->
      badOverlaps

    (_, Similar _ _) : otherFieldPairs ->
      findBadOverlapsHelp badOverlaps otherFieldPairs

    (field, Different aDoc bDoc problems) : otherFieldPairs ->
      findBadOverlapsHelp (BadOverlap field aDoc bDoc problems : badOverlaps) otherFieldPairs
