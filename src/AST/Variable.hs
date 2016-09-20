module AST.Variable where

import Data.Binary
import qualified Data.Maybe as Maybe

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName



-- RAW NAMES


newtype Raw = Raw String
    deriving (Eq, Ord)



-- CANONICAL NAMES


data Home
    = BuiltIn
    | Module ModuleName.Canonical
    | TopLevel ModuleName.Canonical
    | Local
    deriving (Eq, Ord)


data Canonical = Canonical
    { home :: !Home
    , name :: !String
    }
    deriving (Eq, Ord)


local :: String -> Canonical
local x =
    Canonical Local x


topLevel :: ModuleName.Canonical -> String -> Canonical
topLevel home x =
    Canonical (TopLevel home) x


builtin :: String -> Canonical
builtin x =
    Canonical BuiltIn x


fromModule :: ModuleName.Canonical -> String -> Canonical
fromModule home name =
    Canonical (Module home) name


inCore :: ModuleName.Raw -> String -> Canonical
inCore home name =
    Canonical (Module (ModuleName.inCore home)) name


inHtml :: ModuleName.Raw -> String -> Canonical
inHtml home name =
    Canonical (Module (ModuleName.inHtml home)) name


cmd :: Canonical
cmd =
  inCore ["Platform","Cmd"] "Cmd"


sub :: Canonical
sub =
  inCore ["Platform","Sub"] "Sub"



-- VARIABLE RECOGNIZERS


isLocalHome :: Home -> Bool
isLocalHome home =
  case home of
    BuiltIn ->
        False

    Module _ ->
        False

    TopLevel _ ->
        True

    Local ->
        True


is :: ModuleName.Raw -> String -> Canonical -> Bool
is home name var =
    var == inCore home name


isJson :: Canonical -> Bool
isJson =
    is ["Json", "Encode"] "Value"


isMaybe :: Canonical -> Bool
isMaybe =
    is ["Maybe"] "Maybe"

isArray :: Canonical -> Bool
isArray =
    is ["Array"] "Array"


isTask :: Canonical -> Bool
isTask =
    is ["Task"] "Task"


isList :: Canonical -> Bool
isList v =
    v == Canonical BuiltIn "List"


isTuple :: Canonical -> Bool
isTuple v =
    case v of
      Canonical BuiltIn name -> Help.isTuple name
      _ -> False


isPrimitive :: Canonical -> Bool
isPrimitive v =
    case v of
      Canonical BuiltIn name ->
          name `elem` ["Int","Float","String","Bool"]
      _ ->
          False


isPrim :: String -> Canonical -> Bool
isPrim prim (Canonical home name) =
    case home of
      BuiltIn ->
          name == prim

      _ ->
          False


isLocal :: (String -> Bool) -> Canonical -> Bool
isLocal check (Canonical home name) =
  case home of
    Local ->
        check name

    _ ->
        False



-- VARIABLE TO STRING


class ToString a where
  toString :: a -> String


instance ToString Raw where
  toString (Raw name) =
      name


instance ToString Canonical where
  toString (Canonical home name) =
      case home of
        BuiltIn ->
            name

        Module moduleName ->
            ModuleName.canonicalToString moduleName ++ "." ++ name

        TopLevel _ ->
            name

        Local ->
            name



-- LISTINGS


-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a = Listing
    { _explicits :: [a]
    , _open :: Bool
    }
    deriving (Eq, Ord)


openListing :: Listing a
openListing =
    Listing [] True


closedListing :: Listing a
closedListing =
    Listing [] False


listing :: [a] -> Listing a
listing xs =
    Listing xs False


-- | A value that can be imported or exported
data Value
    = Value !String
    | Alias !String
    | Union !String !(Listing String)
    deriving (Eq, Ord)



-- CATEGORIZING VALUES


getValues :: [Value] -> [String]
getValues values =
  Maybe.mapMaybe getValue values


getValue :: Value -> Maybe String
getValue value =
  case value of
    Value name -> Just name
    Alias _ -> Nothing
    Union _ _ -> Nothing


getAliases :: [Value] -> [String]
getAliases values =
  Maybe.mapMaybe getAlias values


getAlias :: Value -> Maybe String
getAlias value =
  case value of
    Value _-> Nothing
    Alias name -> Just name
    Union _ _ -> Nothing


getUnions :: [Value] -> [(String, Listing String)]
getUnions values =
  Maybe.mapMaybe getUnion values


getUnion :: Value -> Maybe (String, Listing String)
getUnion value =
  case value of
    Value _ -> Nothing
    Alias _ -> Nothing
    Union name ctors -> Just (name, ctors)



-- BINARY SERIALIZATION


instance Binary Canonical where
    put (Canonical home name) =
        case home of
          BuiltIn ->
              putWord8 0 >> put name

          Module path ->
              putWord8 1 >> put path >> put name

          TopLevel path ->
              putWord8 2 >> put path >> put name

          Local ->
              putWord8 3 >> put name

    get =
      do  tag <- getWord8
          case tag of
            0 -> Canonical BuiltIn <$> get
            1 -> Canonical . Module <$> get <*> get
            2 -> Canonical . TopLevel <$> get <*> get
            3 -> Canonical Local <$> get
            _ -> error "Unexpected tag when deserializing canonical variable"


instance Binary Value where
    put portable =
        case portable of
          Value name ->
              putWord8 0 >> put name

          Alias name ->
              putWord8 1 >> put name

          Union name ctors ->
              putWord8 2 >> put name >> put ctors

    get =
      do  tag <- getWord8
          case tag of
            0 -> Value <$> get
            1 -> Alias <$> get
            2 -> Union <$> get <*> get
            _ -> error "Error reading valid import/export information from serialized string"


instance (Binary a) => Binary (Listing a) where
    put (Listing explicits open) =
        put explicits >> put open

    get = Listing <$> get <*> get
