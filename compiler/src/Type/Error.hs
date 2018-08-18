{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Error
  ( Type(..)
  , Super(..)
  , Extension(..)
  , iteratedDealias
  , toDoc
  , Problem(..)
  , Direction(..)
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
import Data.Monoid ((<>))

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



-- TO DOC


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
        (L.toDoc localizer home name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record (fieldsToDocs localizer fields) (extToDoc ext)

    Unit ->
      "()"

    Tuple a b maybeC ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (map (toDoc localizer RT.None) (Maybe.maybeToList maybeC))

    Alias home name args _ ->
      aliasToDoc localizer ctx home name args


aliasToDoc :: L.Localizer -> RT.Context -> ModuleName.Canonical -> N.Name -> [(N.Name, Type)] -> D.Doc
aliasToDoc localizer ctx home name args =
  RT.apply ctx
    (L.toDoc localizer home name)
    (map (toDoc localizer RT.App . snd) args)


fieldsToDocs :: L.Localizer -> Map.Map N.Name Type -> [(D.Doc, D.Doc)]
fieldsToDocs localizer fields =
  Map.foldrWithKey (addField localizer) [] fields


addField :: L.Localizer -> N.Name -> Type -> [(D.Doc, D.Doc)] -> [(D.Doc, D.Doc)]
addField localizer fieldName fieldType docs =
  let
    f = D.fromName fieldName
    t = toDoc localizer RT.None fieldType
  in
  (f,t) : docs


extToDoc :: Extension -> Maybe D.Doc
extToDoc ext =
  case ext of
    Closed -> Nothing
    FlexOpen x -> Just (D.fromName x)
    RigidOpen x -> Just (D.fromName x)



-- DIFF


data Diff a =
  Diff a a Status


data Status
  = Similar
  | Different (Bag.Bag Problem)


data Problem
  = IntFloat
  | StringFromInt
  | StringFromFloat
  | StringToInt
  | StringToFloat
  | AnythingToBool
  | AnythingFromMaybe
  | AnythingToList
  | MissingArgs Int
  | ReturnMismatch
  | BadFlexSuper Direction Super N.Name Type
  | BadRigidVar N.Name Type
  | BadRigidSuper Super N.Name Type
  | FieldTypo N.Name [N.Name]
  | FieldsMissing [N.Name]


data Direction = Have | Need


instance Functor Diff where
  fmap func (Diff a b status) =
    Diff (func a) (func b) status


instance Applicative Diff where
  pure a =
    Diff a a Similar

  (<*>) (Diff aFunc bFunc status1) (Diff aArg bArg status2) =
    Diff (aFunc aArg) (bFunc bArg) (merge status1 status2)


merge :: Status -> Status -> Status
merge status1 status2 =
  case status1 of
    Similar ->
      status2

    Different problems1 ->
      case status2 of
        Similar ->
          status1

        Different problems2 ->
          Different (Bag.append problems1 problems2)



-- COMPARISON


toComparison :: L.Localizer -> Type -> Type -> (D.Doc, D.Doc, [Problem])
toComparison localizer tipe1 tipe2 =
  case toDiff localizer RT.None tipe1 tipe2 of
    Diff doc1 doc2 Similar ->
      (doc1, doc2, [])

    Diff doc1 doc2 (Different problems) ->
      (doc1, doc2, Bag.toList problems)


toDiff :: L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (Unit    , Unit    ) -> same localizer ctx tipe1
    (Error   , Error   ) -> same localizer ctx tipe1
    (Infinite, Infinite) -> same localizer ctx tipe1

    (FlexVar      x, FlexVar      y) | x == y -> same localizer ctx tipe1
    (FlexSuper _  x, FlexSuper _  y) | x == y -> same localizer ctx tipe1
    (RigidVar     x, RigidVar     y) | x == y -> same localizer ctx tipe1
    (RigidSuper _ x, RigidSuper _ y) | x == y -> same localizer ctx tipe1

    (FlexVar _, _        ) -> similar localizer ctx tipe1 tipe2
    (_        , FlexVar _) -> similar localizer ctx tipe1 tipe2

    (FlexSuper s _, t            ) | isSuper s t -> similar localizer ctx tipe1 tipe2
    (t            , FlexSuper s _) | isSuper s t -> similar localizer ctx tipe1 tipe2

    (Lambda a b cs, Lambda x y zs) ->
      if length cs == length zs then
        RT.lambda ctx
          <$> toDiff localizer RT.Func a x
          <*> toDiff localizer RT.Func b y
          <*> sequenceA (zipWith (toDiff localizer RT.Func) cs zs)
      else
        diffLambda localizer ctx a b cs x y zs

    (Tuple a b Nothing, Tuple x y Nothing) ->
      RT.tuple
        <$> toDiff localizer RT.None a x
        <*> toDiff localizer RT.None b y
        <*> pure []

    (Tuple a b (Just c), Tuple x y (Just z)) ->
      RT.tuple
        <$> toDiff localizer RT.None a x
        <*> toDiff localizer RT.None b y
        <*> ((:[]) <$> toDiff localizer RT.None c z)

    (Record fields1 ext1, Record fields2 ext2) ->
      diffRecord localizer fields1 ext1 fields2 ext2

    (Type home1 name1 args1, Type home2 name2 args2) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (L.toDoc localizer home1 name1)
        <$> sequenceA (zipWith (toDiff localizer RT.App) args1 args2)

    (Alias home1 name1 args1 _, Alias home2 name2 args2 _) | home1 == home2 && name1 == name2 ->
      RT.apply ctx (L.toDoc localizer home1 name1)
        <$> sequenceA (zipWith (toDiff localizer RT.App) (map snd args1) (map snd args2))

    -- start trying to find specific problems

    (Type home1 name1 args1, Type home2 name2 args2) | L.toString localizer home1 name1 == L.toString localizer home2 name2 ->
      different
        (nameClashToDoc ctx localizer home1 name1 args1)
        (nameClashToDoc ctx localizer home2 name2 args2)
        Bag.empty

    (Type home name [t1], t2) | isMaybe home name && isSimilar (toDiff localizer ctx t1 t2) ->
      different
        (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [toDoc localizer RT.App t1])
        (toDoc localizer ctx t2)
        (Bag.one AnythingFromMaybe)

    (t1, Type home name [t2]) | isList home name && isSimilar (toDiff localizer ctx t1 t2) ->
      different
        (toDoc localizer ctx t1)
        (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [toDoc localizer RT.App t2])
        (Bag.one AnythingToList)

    (Alias home1 name1 args1 t1, t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just (Diff _ doc2 status) ->
          Diff (D.dullyellow (aliasToDoc localizer ctx home1 name1 args1)) doc2 status

        Nothing ->
          case t2 of
            Type home2 name2 args2 | L.toString localizer home1 name1 == L.toString localizer home2 name2 ->
              different
                (nameClashToDoc ctx localizer home1 name1 (map snd args1))
                (nameClashToDoc ctx localizer home2 name2 args2)
                Bag.empty

            _ ->
              different
                (D.dullyellow (toDoc localizer ctx tipe1))
                (D.dullyellow (toDoc localizer ctx tipe2))
                Bag.empty

    (t1, Alias home2 name2 args2 t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just (Diff doc1 _ status) ->
          Diff doc1 (D.dullyellow (aliasToDoc localizer ctx home2 name2 args2)) status

        Nothing ->
          case t1 of
            Type home1 name1 args1 | L.toString localizer home1 name1 == L.toString localizer home2 name2 ->
              different
                (nameClashToDoc ctx localizer home1 name1 args1)
                (nameClashToDoc ctx localizer home2 name2 (map snd args2))
                Bag.empty

            _ ->
              different
                (D.dullyellow (toDoc localizer ctx tipe1))
                (D.dullyellow (toDoc localizer ctx tipe2))
                Bag.empty

    pair ->
      let
        doc1 = D.dullyellow (toDoc localizer ctx tipe1)
        doc2 = D.dullyellow (toDoc localizer ctx tipe2)
      in
      different doc1 doc2 $
        case pair of
          (RigidVar     x, other) -> Bag.one $ BadRigidVar x other
          (FlexSuper  s x, other) -> Bag.one $ BadFlexSuper Have s x other
          (RigidSuper s x, other) -> Bag.one $ BadRigidSuper s x other
          (other, RigidVar     x) -> Bag.one $ BadRigidVar x other
          (other, FlexSuper  s x) -> Bag.one $ BadFlexSuper Need s x other
          (other, RigidSuper s x) -> Bag.one $ BadRigidSuper s x other

          (Type home1 name1 [], Type home2 name2 [])
            | isInt   home1 name1 && isFloat  home2 name2 -> Bag.one IntFloat
            | isFloat home1 name1 && isInt    home2 name2 -> Bag.one IntFloat
            | isInt   home1 name1 && isString home2 name2 -> Bag.one StringFromInt
            | isFloat home1 name1 && isString home2 name2 -> Bag.one StringFromFloat
            | isString home1 name1 && isInt   home2 name2 -> Bag.one StringToInt
            | isString home1 name1 && isFloat home2 name2 -> Bag.one StringToFloat
            | isBool home2 name2 -> Bag.one AnythingToBool

          (_, _) ->
            Bag.empty



-- DIFF HELPERS


same :: L.Localizer -> RT.Context -> Type -> Diff D.Doc
same localizer ctx tipe =
  let
    doc = toDoc localizer ctx tipe
  in
  Diff doc doc Similar


similar :: L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
similar localizer ctx t1 t2 =
  Diff (toDoc localizer ctx t1) (toDoc localizer ctx t2) Similar


different :: a -> a -> Bag.Bag Problem -> Diff a
different a b problems =
  Diff a b (Different problems)


isSimilar :: Diff a -> Bool
isSimilar (Diff _ _ status) =
  case status of
    Similar -> True
    Different _ -> False



-- IS TYPE?


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



-- IS SUPER?


isSuper :: Super -> Type -> Bool
isSuper super tipe =
  case iteratedDealias tipe of
    Type h n args ->
      case super of
        Number     -> isInt h n || isFloat h n
        Comparable -> isInt h n || isFloat h n || isString h n || isChar h n || isList h n && isSuper super (head args)
        Appendable -> isString h n || isList h n
        CompAppend -> isString h n || isList h n && isSuper Comparable (head args)

    _ ->
      False



-- NAME CLASH


nameClashToDoc :: RT.Context -> L.Localizer -> ModuleName.Canonical -> N.Name -> [Type] -> D.Doc
nameClashToDoc ctx localizer (ModuleName.Canonical _ home) name args =
  RT.apply ctx
    (D.yellow (D.fromName home) <> D.dullyellow ("." <> D.fromName name))
    (map (toDoc localizer RT.App) args)



-- DIFF ALIASED RECORD


diffAliasedRecord :: L.Localizer -> Type -> Type -> Maybe (Diff D.Doc)
diffAliasedRecord localizer t1 t2 =
  case (iteratedDealias t1, iteratedDealias t2) of
    (Record fields1 ext1, Record fields2 ext2) ->
      Just (diffRecord localizer fields1 ext1 fields2 ext2)

    _ ->
      Nothing



-- DIFF LAMBDAS


--
-- INVARIANTS:
--   length cs1 /= length cs2
--
diffLambda :: L.Localizer -> RT.Context -> Type -> Type -> [Type] -> Type -> Type -> [Type] -> Diff D.Doc
diffLambda localizer ctx a1 b1 cs1 a2 b2 cs2 =
  let
    (result1:revArgs1) = reverse (a1:b1:cs1)
    (result2:revArgs2) = reverse (a2:b2:cs2)

    numArgs1 = length revArgs1
    numArgs2 = length revArgs2
  in
  case toDiff localizer RT.Func result1 result2 of
    Diff resultDoc1 resultDoc2 Similar ->
      if numArgs1 < numArgs2 then
        diffArgMismatch localizer ctx revArgs1 resultDoc1 revArgs2 resultDoc2

      else
        diffArgMismatch localizer ctx revArgs2 resultDoc2 revArgs1 resultDoc1

    Diff resultDoc1 resultDoc2 (Different problems) ->
      let
        (a:b:cs) = reverse (resultDoc1 : map (toDoc localizer RT.Func) revArgs1)
        (x:y:zs) = reverse (resultDoc2 : map (toDoc localizer RT.Func) revArgs2)
      in
      different
        (D.dullyellow (RT.lambda ctx a b cs))
        (D.dullyellow (RT.lambda ctx x y zs))
        (Bag.append problems (Bag.one ReturnMismatch))


--
-- INVARIANTS:
--   length shortRevArgs >= 2
--   length longRevArgs >= 2
--   length shortRevArgs < length longRevArgs
--
diffArgMismatch :: L.Localizer -> RT.Context -> [Type] -> D.Doc -> [Type] -> D.Doc -> Diff D.Doc
diffArgMismatch localizer ctx shortRevArgs shortResult longRevArgs longResult =
  let
    (a:b:cs, x:y:zs) =
      case toGreedyMatch localizer shortRevArgs longRevArgs of
        Just (GreedyMatch shortRevArgDocs longRevArgDocs) ->
          ( reverse (shortResult:shortRevArgDocs)
          , reverse (longResult:longRevArgDocs)
          )

        Nothing ->
          case toGreedyMatch localizer (reverse shortRevArgs) (reverse longRevArgs) of
            Just (GreedyMatch shortArgDocs longArgDocs) ->
              ( shortArgDocs ++ [shortResult]
              , longArgDocs ++ [longResult]
              )

            Nothing ->
              let
                toYellowDoc tipe =
                  D.dullyellow (toDoc localizer RT.Func tipe)
              in
              ( reverse (shortResult : map toYellowDoc shortRevArgs)
              , reverse (longResult  : map toYellowDoc longRevArgs )
              )
  in
  different
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
      case toDiff localizer RT.Func x y of
        Diff a b Similar ->
          toGreedyMatchHelp localizer xs ys $
            GreedyMatch (a:shorterDocs) (b:longerDocs)

        Diff _ _ (Different _) ->
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
    toUnknownDocs field tipe =
      ( D.dullyellow (D.fromName field), toDoc localizer RT.None tipe )

    toOverlapDocs field t1 t2 =
      (,) (D.fromName field) <$> toDiff localizer RT.None t1 t2

    left = Map.mapWithKey toUnknownDocs (Map.difference fields1 fields2)
    both = Map.intersectionWithKey toOverlapDocs fields1 fields2
    right = Map.mapWithKey toUnknownDocs (Map.difference fields2 fields1)

    fieldsDiff =
      Map.elems <$>
        if Map.null left && Map.null right then
          sequenceA both
        else
          Map.union
            <$> sequenceA both
            <*> Diff left right (Different Bag.empty)

    (Diff doc1 doc2 status) =
      RT.record
        <$> fieldsDiff
        <*> extToDiff ext1 ext2
  in
  Diff doc1 doc2 $ merge status $
    case (hasFixedFields ext1, hasFixedFields ext2) of
      (True, True) ->
        case Map.lookupMin left of
          Just (f,_) -> Different $ Bag.one $ FieldTypo f (Map.keys fields2)
          Nothing ->
            if Map.null right
              then Similar
              else Different $ Bag.one $ FieldsMissing (Map.keys right)

      (False, True) ->
        case Map.lookupMin left of
          Just (f,_) -> Different $ Bag.one $ FieldTypo f (Map.keys fields2)
          Nothing    -> Similar

      (True, False) ->
        case Map.lookupMin right of
          Just (f,_) -> Different $ Bag.one $ FieldTypo f (Map.keys fields1)
          Nothing    -> Similar

      (False, False) ->
        Similar


hasFixedFields :: Extension -> Bool
hasFixedFields ext =
  case ext of
    Closed      -> True
    FlexOpen _  -> False
    RigidOpen _ -> True



-- DIFF RECORD EXTENSION


extToDiff :: Extension -> Extension -> Diff (Maybe D.Doc)
extToDiff ext1 ext2 =
  let
    status = extToStatus ext1 ext2
    extDoc1 = extToDoc ext1
    extDoc2 = extToDoc ext2
  in
  case status of
    Similar ->
      Diff extDoc1 extDoc2 status

    Different _ ->
      Diff (D.dullyellow <$> extDoc1) (D.dullyellow <$> extDoc2) status


extToStatus :: Extension -> Extension -> Status
extToStatus ext1 ext2 =
  case ext1 of
    Closed ->
      case ext2 of
        Closed      -> Similar
        FlexOpen  _ -> Similar
        RigidOpen _ -> Different Bag.empty

    FlexOpen _ ->
      Similar

    RigidOpen x ->
      case ext2 of
        Closed      -> Different Bag.empty
        FlexOpen  _ -> Similar
        RigidOpen y ->
          if x == y
            then Similar
            else Different $ Bag.one $ BadRigidVar x (RigidVar y)
