{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Error
  ( Type(..)
  , Extension(..)
  , iteratedDealias
  , Localizer
  , toDoc
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Module.Name as ModuleName
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


type Localizer = Map.Map (ModuleName.Canonical, N.Name) (Maybe N.Name)


toDoc :: Localizer -> RT.Context -> Type -> H.Doc
toDoc localizer context tipe =
  case tipe of
    Lambda a b cs ->
      RT.lambda context
        (toDoc localizer RT.Func a)
        (toDoc localizer RT.Func b)
        (map (toDoc localizer RT.Func) cs)

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
      RT.apply context
        (toLocal localizer home name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      let
        entryToDocs (fieldName, fieldType) =
          ( H.nameToDoc fieldName, toDoc localizer RT.None fieldType )

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
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (map (toDoc localizer RT.None) (Maybe.maybeToList maybeC))

    Alias home name args _ ->
      RT.apply context
        (toLocal localizer home name)
        (map (toDoc localizer RT.App . snd) args)


toLocal :: Localizer -> ModuleName.Canonical -> N.Name -> H.Doc
toLocal localizer home name =
  error "TODO toLocal" localizer home name



{-- DIFF


data Diff
  = TooFewArgs
  |




-- DETECT MISSING ARGS


detectMissingArgs :: Type -> Type -> Maybe (Type, [Type])
detectMissingArgs actual expected =
  detectMissingArgsHelp (reverseArgs actual) (reverseArgs expected)


detectMissingArgsHelp :: [Type] -> [Type] -> Maybe (Type, [Type])
detectMissingArgsHelp actuals expecteds =
  case (actuals, expecteds) of
    (x:xs, y:ys) -> if isMatchy x y then detectMissingArgsHelp xs ys else Nothing
    (x:xs, [])   -> Just (x, xs)
    ([], [])     -> Nothing
    ([], _:_)    -> Nothing


reverseArgs :: Type -> [Type]
reverseArgs tipe =
  case tipe of
    Lambda a b cs -> reverse (a:b:cs)
    Infinite      -> [tipe]
    Error         -> [tipe]
    FlexVar _     -> [tipe]
    FlexSuper _   -> [tipe]
    RigidVar _    -> [tipe]
    RigidSuper _  -> [tipe]
    Type _ _ _    -> [tipe]
    Record _ _    -> [tipe]
    Unit          -> [tipe]
    Tuple _ _ _   -> [tipe]
    Alias _ _ _ t -> reverseArgs t


isMatchy :: Type -> Type -> Bool
isMatchy t1 t2 =
  case (t1, t2) of
    (Lambda a b cs, Lambda x y zs) ->
      isMatchy a x && isMatchy b y && length cs == length zs && and (zipWith isMatchy cs zs)

    (Infinite, Infinite) ->
      True

    (Error, Error) ->
      True

    (FlexVar _, _) ->
      True

    (_, FlexVar _) ->
      True

    (FlexSuper x, FlexSuper y) ->
      x == y

    (RigidVar x, RigidVar y) ->
      x == y

    (RigidSuper x, RigidSuper y) ->
      x == y

    (Type home1 name1 args1, Type home2 name2 args2) ->
      home1 == home2 && name1 == name2 && and (zipWith isMatchy args1 args2)

    (Record fields1 ext1, Record fields2 ext2) ->
      isMatchRecord
        (Map.size (Map.difference fields1 fields2))
        (Map.intersectionWith isMatchy fields1 fields2)
        (Map.size (Map.difference fields2 fields1))

    (Unit, Unit) ->
      True

    (Tuple a b Nothing, Tuple x y Nothing) ->
      isMatchy a x && isMatchy b y

    (Tuple a b (Just c), Tuple x y (Just z)) ->
      isMatchy a x && isMatchy b y && isMatchy c z

    (Alias _ _ _ alias1, tipe2) ->
      isMatchy alias1 tipe2

    (tipe1, Alias _ _ _ alias2) ->
      isMatchy tipe1 alias2

    (_, _) ->
      False
-}