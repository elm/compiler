module Type.Error
  ( Type(..)
  , Extension(..)
  , iteratedDealias
  , Localizer
  )
  where


import qualified Data.Map as Map

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N



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



-- LOCALIZE


type Localizer = Map.Map N.Name ()




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