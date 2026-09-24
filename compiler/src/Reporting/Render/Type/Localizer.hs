{-# LANGUAGE OverloadedStrings #-}
module Reporting.Render.Type.Localizer
  ( Localizer
  , toDoc
  , toChars
  , empty
  , fromNames
  , fromModule
  )
  where


import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Doc as D
import qualified Reporting.Annotation as A



-- LOCALIZER


newtype Localizer =
  Localizer (Map.Map Module.Name Import)


data Import =
  Import
    { _alias :: Maybe Module.Prefix
    , _exposing :: Exposing
    }


data Exposing
  = All
  | Only (Set.Set T.Name)


empty :: Localizer
empty =
  Localizer Map.empty



-- LOCALIZE


toDoc :: Localizer -> ModuleName.Canonical -> T.Name -> D.Doc
toDoc localizer home name =
  D.fromChars (toChars localizer home name)


toChars :: Localizer -> ModuleName.Canonical -> T.Name -> String
toChars (Localizer localizer) moduleName@(ModuleName.Canonical _ home) name =
  case Map.lookup home localizer of
    Nothing ->
      Module.toChars home <> "." <> T.nameToChars name

    Just (Import alias exposing) ->
      case exposing of
        All ->
          T.nameToChars name

        Only set ->
          if Set.member name set then
            T.nameToChars name
          else if name == T.list && moduleName == ModuleName.list then
            "List"
          else
            case alias of
              Nothing -> Module.toChars home <> "." <> T.nameToChars name
              Just h  -> Module.prefixToChars h <> "." <> T.nameToChars name


-- FROM NAMES


fromNames :: Map.Map Module.Name a -> Localizer
fromNames names =
  Localizer $ Map.map (\_ -> Import Nothing All) names



-- FROM MODULE


fromModule :: Src.Module -> Localizer
fromModule modul@(Src.Module _ _ _ imports _ _ _ _ _) =
  Localizer $ Map.fromList $
    (Src.getName modul, Import Nothing All) : map toPair imports


toPair :: Src.Import -> (Module.Name, Import)
toPair (Src.Import (A.At _ name) alias exposing) =
  ( name
  , Import alias (toExposing exposing)
  )


toExposing :: Src.Exposing -> Exposing
toExposing exposing =
  case exposing of
    Src.Open ->
      All

    Src.Explicit exposedList ->
      Only (foldr addType Set.empty exposedList)


addType :: Src.Exposed -> Set.Set T.Name -> Set.Set T.Name
addType exposed types =
  case exposed of
    Src.Lower _               -> types
    Src.Upper (A.At _ name) _ -> Set.insert name types
    Src.Operator _ _          -> types
