{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Error
  ( Type(..)
  , Super(..)
  , Extension(..)
  , iteratedDealias
  , toDoc
  , toDiffDocs
  , Problem(..)
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


toDiffDocs :: L.Localizer -> Type -> Type -> (D.Doc, D.Doc, [Problem])
toDiffDocs localizer a b =
  case diff localizer RT.None a b of
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



-- COMPUTE DIFF


diff :: L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
diff localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (FlexVar      x, FlexVar      y) | x == y -> pure (D.fromName x)
    (FlexSuper _  x, FlexSuper _  y) | x == y -> pure (D.fromName x)
    (RigidVar     x, RigidVar     y) | x == y -> pure (D.fromName x)
    (RigidSuper _ x, RigidSuper _ y) | x == y -> pure (D.fromName x)

    (Infinite, Infinite) -> pure "∞"
    (Error, Error) -> pure "?"
    (Unit, Unit) -> pure "()"

    (Tuple a b maybeC, Tuple x y maybeZ) ->
        RT.tuple
          <$> diff localizer RT.None a x
          <*> diff localizer RT.None b y
          <*>
            case (maybeC, maybeZ) of
              (Nothing, Nothing) -> pure []
              (Just c , Nothing) -> Different [D.dullyellow (toDoc localizer RT.None c)] [] Bag.empty
              (Nothing, Just z ) -> Different [] [D.dullyellow (toDoc localizer RT.None z)] Bag.empty
              (Just c , Just z ) -> (:[]) <$> diff localizer RT.None c z

    (Record fields1 ext1, Record fields2 ext2) ->
      diffRecord localizer fields1 ext1 fields2 ext2

    (Type home1 name1 args1, Type home2 name2 args2) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (nameToDoc localizer home1 name1) <$>
        sequenceA (zipWith (diff localizer RT.App) args1 args2)

    (Alias home1 name1 args1 _, Alias home2 name2 args2 _) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (nameToDoc localizer home1 name1) <$>
        sequenceA (zipWith (diff localizer RT.App) (map snd args1) (map snd args2))

    (Alias _ _ _ alias1, other) -> diff localizer ctx alias1 other
    (other, Alias _ _ _ alias2) -> diff localizer ctx other alias2

    (Lambda a b cs, Lambda x y zs) -> diffLambda localizer ctx (a:b:cs) (x:y:zs)

    (FlexVar x, other) -> Similar (D.fromName x) (toDoc localizer ctx other)
    (other, FlexVar x) -> Similar (toDoc localizer ctx other) (D.fromName x)

    pair ->
      case pair of
        (Type home name [t1], t2) | isMaybe home name && isSimilar t1 t2 ->
          Different (yellowApply localizer ctx home name t1) (toDoc localizer ctx t2) (Bag.one AnythingFromMaybe)

        (t1, Type home name [t2]) | isList home name && isSimilar t1 t2 ->
          Different (toDoc localizer ctx t1) (yellowApply localizer ctx home name t2) (Bag.one AnythingToList)

        _ ->
          let
            doc1 = D.dullyellow (toDoc localizer ctx tipe1)
            doc2 = D.dullyellow (toDoc localizer ctx tipe2)
          in
          Different doc1 doc2 $
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


isSimilar :: Type -> Type -> Bool
isSimilar tipe1 tipe2 =
  case diff L.empty RT.None tipe1 tipe2 of
    Similar _ _ -> True
    Different _ _ _ -> False


yellowApply :: L.Localizer -> RT.Context -> ModuleName.Canonical -> N.Name -> Type -> D.Doc
yellowApply localizer ctx home name tipe =
  RT.apply ctx
    (D.dullyellow (nameToDoc localizer home name))
    [toDoc localizer RT.App tipe]



-- DIFF LAMBDAS


--
-- INVARIANT: length types1 >= 2 && length types2 >= 2
--
diffLambda :: L.Localizer -> RT.Context -> [Type] -> [Type] -> Diff D.Doc
diffLambda localizer ctx types1 types2 =
  let
    (result1:revArgs1) = reverse types1
    (result2:revArgs2) = reverse types2

    numArgs1 = length revArgs1
    numArgs2 = length revArgs2

    resultDiff = diff localizer RT.Func result1 result2
  in
  case resultDiff of
    Similar resultDoc1 resultDoc2 ->
      if numArgs1 == numArgs2 then
        (\resultDoc revArgsDoc ->
            let (x:y:zs) = reverse (resultDoc:revArgsDoc) in
            RT.lambda ctx x y zs
        )
          <$> resultDiff
          <*> sequenceA (zipWith (diff localizer RT.Func) revArgs1 revArgs2)

      else if numArgs1 < numArgs2 then
        diffArgMismatch localizer ctx revArgs1 resultDoc1 revArgs2 resultDoc2

      else
        diffArgMismatch localizer ctx revArgs2 resultDoc2 revArgs1 resultDoc1

    Different resultDoc1 resultDoc2 problems ->
      let
        (x1:y1:zs1) = reverse (resultDoc1 : map (toDoc localizer RT.Func) revArgs1)
        (x2:y2:zs2) = reverse (resultDoc2 : map (toDoc localizer RT.Func) revArgs2)
      in
      Different
        (RT.lambda ctx x1 y1 zs1)
        (RT.lambda ctx x2 y2 zs2)
        (Bag.append (Bag.one ReturnMismatch) problems)


--
-- INVARIANT: length shortRevArgs >= 2 && length longRevArgs >= 2
--
diffArgMismatch :: L.Localizer -> RT.Context -> [Type] -> D.Doc -> [Type] -> D.Doc -> Diff D.Doc
diffArgMismatch localizer ctx shortRevArgs shortResult longRevArgs longResult =
  case toGreedyMatch localizer shortRevArgs longRevArgs of
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
      case toGreedyMatch localizer (reverse shortRevArgs) (reverse longRevArgs) of
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
              D.dullyellow (toDoc localizer RT.Func tipe)

            (a:b:cs) = reverse (shortResult : map toYellowDoc shortRevArgs)
            (x:y:zs) = reverse (longResult  : map toYellowDoc longRevArgs )
          in
          Different
            (RT.lambda ctx a b cs)
            (RT.lambda ctx x y zs)
            (Bag.one (MissingArgs (length longRevArgs - length shortRevArgs)))



-- GREEDY ARG MATCHER


data GreedyMatch =
  GreedyMatch [D.Doc] [D.Doc]


--
-- INVARIANT: length shorterArgs < length longerArgs
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
      case diff localizer RT.Func x y of
        Similar a b ->
          toGreedyMatchHelp localizer xs ys $
            GreedyMatch (a:shorterDocs) (b:longerDocs)

        Different _ _ _ ->
          toGreedyMatchHelp localizer shorterArgs ys $
            GreedyMatch shorterDocs (toYellowDoc y : longerDocs)

    ([], []) ->
      Just match

    ([], _:_) ->
      Just (GreedyMatch shorterDocs (map toYellowDoc longerArgs))

    (_:_, []) ->
      Nothing



-- RECORD DIFFS


diffRecord :: L.Localizer -> Map.Map N.Name Type -> Extension -> Map.Map N.Name Type -> Extension -> Diff D.Doc
diffRecord localizer fields1 ext1 fields2 ext2 =
  let
    only1 = Map.keys (Map.difference fields1 fields2)
    only2 = Map.keys (Map.difference fields2 fields1)
  in
  if null only1 && null only2 then
    case findBadOverlaps localizer fields1 fields2 of
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


toMissingDoc :: Map.Map N.Name t -> [N.Name] -> Extension -> D.Doc
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


emphasizeFieldName :: N.Name -> (D.Doc, D.Doc)
emphasizeFieldName field =
  ( D.dullyellow (D.fromName field), "..." )


toBoringRecord :: Map.Map N.Name t -> Extension -> D.Doc
toBoringRecord fields ext =
  case ext of
    Closed      -> if Map.null fields then "{}" else "{ ... }"
    FlexOpen _  -> "{ ... }"
    RigidOpen _ -> "{ ... }"



-- DIFF RECORD EXTENSION


diffExt :: Extension -> Extension -> Diff (Maybe D.Doc)
diffExt ext1 ext2 =
  let
    normal = Just . D.fromName
    yellow = Just . D.dullyellow . D.fromName

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
    , _doc1 :: D.Doc
    , _doc2 :: D.Doc
    , _problems :: Bag.Bag Problem
    }


findBadOverlaps :: L.Localizer -> Map.Map N.Name Type -> Map.Map N.Name Type -> [BadOverlap]
findBadOverlaps localizer fields1 fields2 =
  findBadOverlapsHelp [] $ Map.toList $
    Map.intersectionWith (diff localizer RT.None) fields1 fields2


findBadOverlapsHelp :: [BadOverlap] -> [(N.Name, Diff D.Doc)] -> [BadOverlap]
findBadOverlapsHelp badOverlaps fieldPairs =
  case fieldPairs of
    [] ->
      badOverlaps

    (_, Similar _ _) : otherFieldPairs ->
      findBadOverlapsHelp badOverlaps otherFieldPairs

    (field, Different aDoc bDoc problems) : otherFieldPairs ->
      findBadOverlapsHelp (BadOverlap field aDoc bDoc problems : badOverlaps) otherFieldPairs
