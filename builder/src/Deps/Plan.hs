module Install.Plan where

import qualified Data.Map as Map

import qualified Elm.Package.Solution as S
import qualified Elm.Package as Package


data Plan = Plan
    { installs :: Map.Map Package.Name Package.Version
    , upgrades :: Map.Map Package.Name (Package.Version, Package.Version)
    , removals :: Map.Map Package.Name Package.Version
    }


create :: S.Solution -> S.Solution -> Plan
create old new =
    Plan
    { installs = Map.difference new old
    , upgrades = discardNoOps (Map.intersectionWith (,) old new)
    , removals = Map.difference old new
    }
  where
    discardNoOps updates =
      Map.mapMaybe isChanged updates

    isChanged upgrade@(oldVersion,newVersion) =
      if oldVersion == newVersion
        then Nothing
        else Just upgrade


isEmpty :: Plan -> Bool
isEmpty (Plan installs upgrades removals) =
    Map.null installs
        && Map.null upgrades
        && Map.null removals


-- DISPLAY

display :: Plan -> String
display (Plan installs upgrades removals) =
    "\n"
    ++ displayCategory "Install" displayInstall installs
    ++ displayCategory "Upgrade" displayUpgrade upgrades
    ++ displayCategory "Remove" displayRemove removals
  where
    displayCategory name render category =
        if Map.null category then "" else
          "  " ++ name ++ ":"
          ++ concatMap (\entry -> "\n    " ++ render entry) (Map.toList category)
          ++ "\n"

    displayInstall (name, version) =
        Package.toString name ++ " " ++ Package.versionToString version

    displayUpgrade (name, (old, new)) =
        Package.toString name ++ " ("
        ++ Package.versionToString old ++ " => " ++ Package.versionToString new ++ ")"

    displayRemove (name, _version) =
        Package.toString name
