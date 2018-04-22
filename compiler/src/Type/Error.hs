{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Error
  ( Type(..)
  , Super(..)
  , Extension(..)
  , iteratedDealias
  , getFields
  , toDoc
  , Comparison(..)
  , Problem(..)
  , toComparison
  , isInt
  , isFloat
  , isString
  , isChar
  , isList
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- ERROR TYPES


data Type
  = Lambda Type Type [Type]
  | Infinite
  | Error
  | FlexVar N.Name
  | FlexSuper Super N.Name
  | RigidVar N.Name
  | RigidSuper Super N.Name
  | Type ModuleName.Canonical N.Name [Type]
  | Record (Map.Map N.Name Type) Extension
  | Unit
  | Tuple Type Type (Maybe Type)
  | Alias ModuleName.Canonical N.Name [(N.Name, Type)] Type


data Super
  = Number
  | Comparable
  | Appendable
  | CompAppend
  deriving (Eq)


data Extension
  = Closed
  | FlexOpen N.Name
  | RigidOpen N.Name


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    Alias _ _ _ real ->
      iteratedDealias real

    _ ->
      tipe


getFields :: Type -> Maybe [N.Name]
getFields tipe =
  case tipe of
    Alias _ _ _ real ->
      getFields real

    Record fields _ ->
      Just (Map.keys fields)

    _ ->
      Nothing



-- TO DOC


nameToDoc :: L.Localizer -> ModuleName.Canonical -> N.Name -> D.Doc
nameToDoc localizer (ModuleName.Canonical _ home) name =
  L.toDoc localizer home name


toDoc :: L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer ctx tipe =
  case tipe of
    Lambda a b cs ->
      RT.lambda ctx
        (toDoc localizer RT.Func a)
        (toDoc localizer RT.Func b)
        (map (toDoc localizer RT.Func) cs)

    Infinite ->
      "∞"

    Error ->
      "?"

    FlexVar name ->
      D.fromName name

    FlexSuper _ name ->
      D.fromName name

    RigidVar name ->
      D.fromName name

    RigidSuper _ name ->
      D.fromName name

    Type home name args ->
      RT.apply ctx
        (nameToDoc localizer home name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      let
        entryToDocs (fieldName, fieldType) =
          ( D.fromName fieldName, toDoc localizer RT.None fieldType )

        fieldDocs =
          map entryToDocs (Map.toList fields)
      in
      RT.record fieldDocs $
        case ext of
          Closed -> Nothing
          FlexOpen x -> Just (D.fromName x)
          RigidOpen x -> Just (D.fromName x)

    Unit ->
      "()"

    Tuple a b maybeC ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (map (toDoc localizer RT.None) (Maybe.maybeToList maybeC))

    Alias home name args _ ->
      RT.apply ctx
        (nameToDoc localizer home name)
        (map (toDoc localizer RT.App . snd) args)



-- DIFF


data Diff
  = Similar Type Type
  | DiffLambda Aliases Diff Diff [Diff]
  | DiffType Aliases ModuleName.Canonical N.Name [Diff]
  | DiffRecord Aliases [N.Name] (Map.Map N.Name Diff) [N.Name] Extension Extension
  | DiffTuple Aliases Diff Diff (Maybe Diff)
  | Clash Type Type


same :: Type -> Diff
same tipe =
  Similar tipe tipe


data Aliases =
  Aliases
    { _left :: Maybe N.Name
    , _right :: Maybe N.Name
    }


{-# NOINLINE noAliases #-}
noAliases :: Aliases
noAliases =
  Aliases Nothing Nothing


toDiff :: Type -> Type -> Diff
toDiff t1 t2 =
  case (t1, t2) of
    (Unit    , Unit    ) -> same t1
    (Error   , Error   ) -> same t1
    (Infinite, Infinite) -> same t1

    (FlexVar      x, FlexVar      y) | x == y -> same t1
    (FlexSuper _  x, FlexSuper _  y) | x == y -> same t1
    (RigidVar     x, RigidVar     y) | x == y -> same t1
    (RigidSuper _ x, RigidSuper _ y) | x == y -> same t1

    (FlexVar _    , _            ) -> Similar t1 t2
    (_            , FlexVar _    ) -> Similar t1 t2

    (Lambda a1 b1 cs1, Lambda a2 b2 cs2) | length cs1 == length cs2 ->
      let
        a = toDiff a1 a2
        b = toDiff b1 b2
        cs = zipWith toDiff cs1 cs2
      in
      if isSimilar a && isSimilar b && all isSimilar cs
        then Similar t1 t2
        else DiffLambda noAliases a b cs

    (Tuple a1 b1 Nothing, Tuple a2 b2 Nothing) ->
      let
        a = toDiff a1 a2
        b = toDiff b1 b2
      in
      if isSimilar a && isSimilar b
        then Similar t1 t2
        else DiffTuple noAliases a b Nothing

    (Tuple a1 b1 (Just c1), Tuple a2 b2 (Just c2)) ->
      let
        a = toDiff a1 a2
        b = toDiff b1 b2
        c = toDiff c1 c2
      in
      if isSimilar a && isSimilar b && isSimilar c
        then Similar t1 t2
        else DiffTuple noAliases a b (Just c)

    (Record fields1 ext1, Record fields2 ext2) ->
      let
        left = Map.keys (Map.difference fields1 fields2)
        both = Map.intersectionWith toDiff fields1 fields2
        right = Map.keys (Map.difference fields2 fields1)
      in
      if null left && null right && all isSimilar both && isExtSimilar ext1 ext2
        then Similar t1 t2
        else DiffRecord noAliases left both right ext1 ext2

    (Type home1 name1 args1, Type home2 name2 args2) | home1 == home2 && name1 == name2 ->
      let
        args = zipWith toDiff args1 args2
      in
      if all isSimilar args
        then Similar t1 t2
        else DiffType noAliases home1 name1 args

    (Alias home1 name1 args1 _, Alias home2 name2 args2 _) | home1 == home2 && name1 == name2 ->
      let
        args = zipWith toDiff (map snd args1) (map snd args2)
      in
      if all isSimilar args
        then Similar t1 t2
        else DiffType noAliases home1 name1 args

    (alias@(Alias _ name _ tipe1), tipe2) ->
      addAlias (const alias) id (\(Aliases _ t) -> Aliases (Just name) t) (toDiff tipe1 tipe2)

    (tipe1, alias@(Alias _ name _ tipe2)) ->
      addAlias id (const alias) (\(Aliases t _) -> Aliases t (Just name)) (toDiff tipe1 tipe2)

    _ ->
      Clash t1 t2


isSimilar :: Diff -> Bool
isSimilar diff =
  case diff of
    Similar _ _            -> True
    DiffLambda _ _ _ _     -> False
    DiffType _ _ _ _       -> False
    DiffRecord _ _ _ _ _ _ -> False
    DiffTuple _ _ _ _      -> False
    Clash _ _              -> False


addAlias :: (Type -> Type) -> (Type -> Type) -> (Aliases -> Aliases) -> Diff -> Diff
addAlias updateLeft updateRight updateAliases diff =
  case diff of
    Similar t1 t2 ->
      Similar (updateLeft t1) (updateRight t2)

    DiffLambda aliases a b cs ->
      DiffLambda (updateAliases aliases) a b cs

    DiffType aliases home name args ->
      DiffType (updateAliases aliases) home name args

    DiffRecord aliases lefts boths rights ext1 ext2 ->
      DiffRecord (updateAliases aliases) lefts boths rights ext1 ext2

    DiffTuple aliases a b maybeC ->
      DiffTuple (updateAliases aliases) a b maybeC

    Clash t1 t2 ->
      Clash (updateLeft t1) (updateRight t2)



-- COMPARISON


data Comparison =
  Comparison
    { _alias1 :: Maybe N.Name
    , _type1 :: D.Doc
    , _alias2 :: Maybe N.Name
    , _type2 :: D.Doc
    , _problems :: [Problem]
    }


data Problem
  = FieldMismatch [N.Name] [N.Name]
  | IntFloat
  | StringFromInt
  | StringFromFloat
  | StringToInt
  | StringToFloat
  | AnythingToBool
  | AnythingFromMaybe
  | AnythingToList
  | MissingArgs Int
  | ReturnMismatch
  | BadFlexSuper Super N.Name Type
  | BadRigidVar N.Name Type
  | BadRigidSuper Super N.Name Type


toComparison :: L.Localizer -> Type -> Type -> Comparison
toComparison localizer a b =
  let
    diff = toDiff a b
    (Aliases aAlias bAlias) = getAliases diff
    (Pair aDoc bDoc problems) = diffToPair localizer RT.None diff
  in
  Comparison aAlias aDoc bAlias bDoc (Bag.toList problems)


getAliases :: Diff -> Aliases
getAliases diff =
  case diff of
    Similar _ _                  -> noAliases
    DiffLambda aliases _ _ _     -> aliases
    DiffType aliases _ _ _       -> aliases
    DiffRecord aliases _ _ _ _ _ -> aliases
    DiffTuple aliases _ _ _      -> aliases
    Clash _ _                    -> noAliases



-- DIFF TO DOCS


data Pair a =
  Pair a a (Bag.Bag Problem)


instance Functor Pair where
  fmap func (Pair a b ps) =
    Pair (func a) (func b) ps


instance Applicative Pair where
  pure a =
    Pair a a Bag.empty

  (<*>) (Pair aFunc bFunc ps1) (Pair aArg bArg ps2) =
    Pair (aFunc aArg) (bFunc bArg) (Bag.append ps1 ps2)


diffToPair :: L.Localizer -> RT.Context -> Diff -> Pair D.Doc
diffToPair localizer ctx diff =
  case diff of
    Similar tipe1 tipe2 ->
      Pair (toDoc localizer ctx tipe1) (toDoc localizer ctx tipe2) Bag.empty

    DiffLambda _ a b cs ->
      RT.lambda ctx
        <$> diffToPair localizer RT.Func a
        <*> diffToPair localizer RT.Func b
        <*> traverse (diffToPair localizer RT.Func) cs

    DiffType _ home name args ->
      RT.apply ctx (nameToDoc localizer home name)
        <$> traverse (diffToPair localizer RT.App) args

    DiffRecord _ left both right ext1 ext2 ->
      diffRecord localizer left both right ext1 ext2

    DiffTuple _ a b maybeC ->
      RT.tuple
        <$> diffToPair localizer RT.None a
        <*> diffToPair localizer RT.None b
        <*> traverse (diffToPair localizer RT.None) (Maybe.maybeToList maybeC)

    Clash tipe1 tipe2 ->
      case (tipe1, tipe2) of
        (Lambda a b cs, Lambda x y zs) ->
          diffLambda localizer ctx (a:b:cs) (x:y:zs)

        (Type home name [t1], t2) | isMaybe home name && isSimilar (toDiff t1 t2) ->
          Pair (yellowApply localizer ctx home name t1) (toDoc localizer ctx t2) (Bag.one AnythingFromMaybe)

        (t1, Type home name [t2]) | isList home name && isSimilar (toDiff t1 t2) ->
          Pair (toDoc localizer ctx t1) (yellowApply localizer ctx home name t2) (Bag.one AnythingToList)

        pair ->
          let
            doc1 = D.dullyellow (toDoc localizer ctx tipe1)
            doc2 = D.dullyellow (toDoc localizer ctx tipe2)
          in
          Pair doc1 doc2 $
            case pair of
              (RigidVar     x, other) -> Bag.one $ BadRigidVar x other
              (FlexSuper  s x, other) -> Bag.one $ BadFlexSuper s x other
              (RigidSuper s x, other) -> Bag.one $ BadRigidSuper s x other
              (other, RigidVar     x) -> Bag.one $ BadRigidVar x other
              (other, FlexSuper  s x) -> Bag.one $ BadFlexSuper s x other
              (other, RigidSuper s x) -> Bag.one $ BadRigidSuper s x other

              (Type home1 name1 [], Type home2 name2 [])
                | isInt   home1 name1 && isFloat  home2 name2 -> Bag.one IntFloat
                | isFloat home1 name1 && isInt    home2 name2 -> Bag.one IntFloat
                | isInt   home1 name1 && isString home2 name2 -> Bag.one StringFromInt
                | isFloat home1 name1 && isString home2 name2 -> Bag.one StringFromFloat
                | isString home1 name1 && isInt   home2 name2 -> Bag.one StringToInt
                | isString home1 name1 && isFloat home2 name2 -> Bag.one StringToFloat
                | isBool home2 name2 -> Bag.one AnythingToBool

              (_, _) -> Bag.empty


isBool :: ModuleName.Canonical -> N.Name -> Bool
isBool home name =
  home == ModuleName.basics && name == N.bool


isInt :: ModuleName.Canonical -> N.Name -> Bool
isInt home name =
  home == ModuleName.basics && name == N.int


isFloat :: ModuleName.Canonical -> N.Name -> Bool
isFloat home name =
  home == ModuleName.basics && name == N.float


isString :: ModuleName.Canonical -> N.Name -> Bool
isString home name =
  home == ModuleName.string && name == N.string


isChar :: ModuleName.Canonical -> N.Name -> Bool
isChar home name =
  home == ModuleName.char && name == N.char


isMaybe :: ModuleName.Canonical -> N.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe && name == N.maybe


isList :: ModuleName.Canonical -> N.Name -> Bool
isList home name =
  home == ModuleName.list && name == N.list


yellowApply :: L.Localizer -> RT.Context -> ModuleName.Canonical -> N.Name -> Type -> D.Doc
yellowApply localizer ctx home name tipe =
  RT.apply ctx
    (D.dullyellow (nameToDoc localizer home name))
    [toDoc localizer RT.App tipe]



-- DIFF LAMBDAS


--
-- INVARIANTS:
--   length types1 >= 2
--   length types2 >= 2
--   length types1 /= length types2
--
diffLambda :: L.Localizer -> RT.Context -> [Type] -> [Type] -> Pair D.Doc
diffLambda localizer ctx types1 types2 =
  let
    (result1:revArgs1) = reverse types1
    (result2:revArgs2) = reverse types2

    numArgs1 = length revArgs1
    numArgs2 = length revArgs2

    diff = toDiff result1 result2

    (Pair r1 r2 _) = diffToPair localizer RT.Func diff
  in
  if isSimilar diff then

    if numArgs1 < numArgs2 then
      diffArgMismatch localizer ctx revArgs1 r1 revArgs2 r2
    else
      diffArgMismatch localizer ctx revArgs2 r2 revArgs1 r1

  else
    let
      (a1:b1:cs1) = reverse (r1 : map (toDoc localizer RT.Func) revArgs1)
      (a2:b2:cs2) = reverse (r2 : map (toDoc localizer RT.Func) revArgs2)
    in
    Pair
      (D.dullyellow (RT.lambda ctx a1 b1 cs1))
      (D.dullyellow (RT.lambda ctx a2 b2 cs2))
      (Bag.one ReturnMismatch)


--
-- INVARIANTS:
--   length shortRevArgs >= 2
--   length longRevArgs >= 2
--
diffArgMismatch :: L.Localizer -> RT.Context -> [Type] -> D.Doc -> [Type] -> D.Doc -> Pair D.Doc
diffArgMismatch localizer ctx shortRevArgs shortResult longRevArgs longResult =
  case toGreedyMatch localizer shortRevArgs longRevArgs of
    Just (GreedyMatch shortRevArgDocs longRevArgDocs) ->
      let
        (a:b:cs) = reverse (shortResult:shortRevArgDocs)
        (x:y:zs) = reverse (longResult:longRevArgDocs)
      in
      Pair
        (RT.lambda ctx a b cs)
        (RT.lambda ctx x y zs)
        (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))

    Nothing ->
      case toGreedyMatch localizer (reverse shortRevArgs) (reverse longRevArgs) of
        Just (GreedyMatch shortArgDocs longArgDocs) ->
          let
            (a:b:cs) = shortArgDocs ++ [shortResult]
            (x:y:zs) = longArgDocs ++ [longResult]
          in
          Pair
            (RT.lambda ctx a b cs)
            (RT.lambda ctx x y zs)
            (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))

        Nothing ->
          let
            toYellowDoc tipe =
              D.dullyellow (toDoc localizer RT.Func tipe)

            (a:b:cs) = reverse (shortResult : map toYellowDoc shortRevArgs)
            (x:y:zs) = reverse (longResult  : map toYellowDoc longRevArgs )
          in
          Pair
            (RT.lambda ctx a b cs)
            (RT.lambda ctx x y zs)
            (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))



-- GREEDY ARG MATCHER


data GreedyMatch =
  GreedyMatch [D.Doc] [D.Doc]


--
-- INVARIANTS:
--   length shorterArgs < length longerArgs
--
toGreedyMatch :: L.Localizer -> [Type] -> [Type] -> Maybe GreedyMatch
toGreedyMatch localizer shorterArgs longerArgs =
  toGreedyMatchHelp localizer shorterArgs longerArgs (GreedyMatch [] [])


toGreedyMatchHelp :: L.Localizer -> [Type] -> [Type] -> GreedyMatch -> Maybe GreedyMatch
toGreedyMatchHelp localizer shorterArgs longerArgs match@(GreedyMatch shorterDocs longerDocs) =
  let
    toYellowDoc tipe =
      D.dullyellow (toDoc localizer RT.Func tipe)
  in
  case (shorterArgs, longerArgs) of
    (x:xs, y:ys) ->
      case toDiff x y of
        Similar a b ->
          toGreedyMatchHelp localizer xs ys $
            GreedyMatch
              (toDoc localizer RT.Func a : shorterDocs)
              (toDoc localizer RT.Func b : longerDocs)

        _ ->
          toGreedyMatchHelp localizer shorterArgs ys $
            GreedyMatch shorterDocs (toYellowDoc y : longerDocs)

    ([], []) ->
      Just match

    ([], _:_) ->
      Just (GreedyMatch shorterDocs (map toYellowDoc longerArgs))

    (_:_, []) ->
      Nothing



-- RECORD DIFFS


diffRecord
  :: L.Localizer
  -> [N.Name]
  -> Map.Map N.Name Diff
  -> [N.Name]
  -> Extension
  -> Extension
  -> Pair D.Doc
diffRecord localizer only1 both only2 ext1 ext2 =
  if null only1 && null only2 then
    case findBadOverlaps localizer both of
      [] ->
        case diffExt ext1 ext2 of
          ExtSimilar ->
            Pair
              (toBoringRecord (Map.size both) ext1)
              (toBoringRecord (Map.size both) ext2)
              Bag.empty

          ExtDifferent maybeExt1 maybeExt2 problems ->
            Pair
              (RT.record [] (fmap (D.dullyellow . D.fromName) maybeExt1))
              (RT.record [] (fmap (D.dullyellow . D.fromName) maybeExt2))
              problems

      overlaps@(bad:bads) ->
        if length overlaps < Map.size both then
          Pair
            (RT.recordSnippet (toOverlapDoc _doc1 bad) (map (toOverlapDoc _doc1) bads))
            (RT.recordSnippet (toOverlapDoc _doc2 bad) (map (toOverlapDoc _doc2) bads))
            (foldr Bag.append Bag.empty (map _problemBag overlaps))
        else
          Pair
            (toOverlapRecord _doc1 bad bads ext1)
            (toOverlapRecord _doc2 bad bads ext2)
            (foldr Bag.append Bag.empty (map _problemBag overlaps))

  else
    Pair
      (toMissingDoc (Map.size both) only1 ext1)
      (toMissingDoc (Map.size both) only2 ext2)
      (Bag.one (FieldMismatch only1 only2))


toOverlapDoc :: (BadOverlap -> D.Doc) -> BadOverlap -> (D.Doc, D.Doc)
toOverlapDoc getDoc overlap@(BadOverlap field _ _ _) =
  (D.fromName field, getDoc overlap)


toOverlapRecord :: (BadOverlap -> D.Doc) -> BadOverlap -> [BadOverlap] -> Extension -> D.Doc
toOverlapRecord getDoc bad bads ext =
  let go = toOverlapDoc getDoc in
  case ext of
    Closed      -> RT.record (map go (bad:bads)) Nothing
    FlexOpen  _ -> RT.recordSnippet (go bad) (map go bads)
    RigidOpen _ -> RT.recordSnippet (go bad) (map go bads)


toMissingDoc :: Int -> [N.Name] -> Extension -> D.Doc
toMissingDoc numSharedFields uniqueFields ext =
  case map emphasizeFieldName uniqueFields of
    [] ->
      toBoringRecord numSharedFields ext

    doc:docs ->
      if length uniqueFields < numSharedFields then
        RT.recordSnippet doc docs
      else
        case ext of
          Closed      -> RT.record (doc:docs) Nothing
          FlexOpen _  -> RT.recordSnippet doc docs
          RigidOpen _ -> RT.recordSnippet doc docs


emphasizeFieldName :: N.Name -> (D.Doc, D.Doc)
emphasizeFieldName field =
  ( D.dullyellow (D.fromName field), "..." )


toBoringRecord :: Int -> Extension -> D.Doc
toBoringRecord numFields ext =
  case ext of
    Closed      -> if numFields == 0 then "{}" else "{ ... }"
    FlexOpen _  -> "{ ... }"
    RigidOpen _ -> "{ ... }"



-- DIFF RECORD EXTENSION


isExtSimilar :: Extension -> Extension -> Bool
isExtSimilar ext1 ext2 =
  case diffExt ext1 ext2 of
    ExtSimilar ->
      True

    ExtDifferent _ _ _ ->
      False


data ExtDiff
  = ExtSimilar
  | ExtDifferent (Maybe N.Name) (Maybe N.Name) (Bag.Bag Problem)


diffExt :: Extension -> Extension -> ExtDiff
diffExt ext1 ext2 =
  let
    different x y = ExtDifferent x y Bag.empty
  in
  case ext1 of
    Closed ->
      case ext2 of
        Closed      -> ExtSimilar
        FlexOpen  y -> different Nothing (Just y)
        RigidOpen y -> different Nothing (Just y)

    FlexOpen x ->
      case ext2 of
        Closed      -> different (Just x) Nothing
        FlexOpen  _ -> ExtSimilar
        RigidOpen _ -> ExtSimilar

    RigidOpen x ->
      case ext2 of
        Closed      -> different (Just x) Nothing
        FlexOpen  _ -> ExtSimilar
        RigidOpen y ->
          if x == y then
            ExtSimilar
          else
            ExtDifferent (Just x) (Just y) (Bag.one (BadRigidVar x (RigidVar y)))



-- BAD OVERLAP


data BadOverlap =
  BadOverlap
    { _field :: N.Name
    , _doc1 :: D.Doc
    , _doc2 :: D.Doc
    , _problemBag :: Bag.Bag Problem
    }


findBadOverlaps :: L.Localizer -> Map.Map N.Name Diff -> [BadOverlap]
findBadOverlaps localizer overlaps =
  Map.foldrWithKey (addBadOverlap localizer) [] overlaps


addBadOverlap :: L.Localizer -> N.Name -> Diff -> [BadOverlap] -> [BadOverlap]
addBadOverlap localizer field diff badOverlaps =
  if isSimilar diff then
    badOverlaps
  else
    let
      (Pair aDoc bDoc problems) = diffToPair localizer RT.None diff
    in
    BadOverlap field aDoc bDoc problems : badOverlaps
