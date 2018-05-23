{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Diff
  ( Args(..)
  , diff
  )
  where


import Control.Monad.Except (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Deps.Cache as Cache
import Deps.Diff (PackageChanges(..), ModuleChanges(..), Changes(..), Magnitude(..))
import qualified Deps.Diff as Diff
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import Reporting.Doc ((<>), (<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Diff as E
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task



-- DIFF


data Args
  = CodeVsLatest
  | CodeVsExactly Pkg.Version
  | LocalInquiry Pkg.Version Pkg.Version
  | GlobalInquiry Pkg.Name Pkg.Version Pkg.Version


diff :: Args -> Task.Task ()
diff args =
  case args of
    GlobalInquiry name v1 v2 ->
      do  registry <- Cache.mandatoryUpdate
          case Cache.getVersions name registry of
            Left suggestions ->
              throw $ E.UnknownPackage name suggestions

            Right vsns ->
              do  oldDocs <- getDocs name vsns (min v1 v2)
                  newDocs <- getDocs name vsns (max v1 v2)
                  writeDiff oldDocs newDocs

    LocalInquiry v1 v2 ->
      do  (_, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (min v1 v2)
          newDocs <- getDocs name vsns (max v1 v2)
          writeDiff oldDocs newDocs

    CodeVsLatest ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (maximum vsns)
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDiff oldDocs newDocs

    CodeVsExactly version ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns version
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDiff oldDocs newDocs


throw :: E.Exit -> Task.Task a
throw exit =
  Task.throw $ Exit.Diff exit



-- DIFF HELPERS


getDocs :: Pkg.Name -> [Pkg.Version] -> Pkg.Version -> Task.Task Docs.Documentation
getDocs name allVersions version =
  if elem version allVersions then
    Cache.getDocs name version
  else
    throw $ E.UnknownVersion name version allVersions


getPackageInfo :: Task.Task (Summary.Summary, Pkg.Name, [Pkg.Version])
getPackageInfo =
  do  summary <- Project.getRoot
      case Summary._project summary of
        Project.App _ ->
          throw $ E.Application

        Project.Pkg (Project.PkgInfo name _ _ _ _ _ _ _) ->
          do  registry <- Cache.mandatoryUpdate
              case Cache.getVersions name registry of
                Right vsns ->
                  return ( summary, name, vsns )

                Left _ ->
                  throw E.Unpublished



-- WRITE DOC


writeDiff :: Docs.Documentation -> Docs.Documentation -> Task.Task ()
writeDiff oldDocs newDocs =
  let
    changes = Diff.diff oldDocs newDocs
    localizer = L.fromNames (Map.union oldDocs newDocs)
  in
  liftIO $ Help.toStdout (toDoc localizer changes <> "\n")



-- TO DOC


toDoc :: L.Localizer -> PackageChanges -> D.Doc
toDoc localizer changes@(PackageChanges added changed removed) =
  if null added && Map.null changed && null removed then
    "No API changes detected, so this is a" <+> D.green "PATCH" <+> "change."
  else
    let
      magDoc =
        D.fromString (Diff.magnitudeToString (Diff.toMagnitude changes))

      header =
        "This is a" <+> D.green magDoc <+> "change."

      addedChunk =
        if null added then [] else
          [ Chunk "ADDED MODULES" MINOR $
              D.vcat $ map (D.fromString . Module.nameToString) added
          ]

      removedChunk =
        if null removed then [] else
          [ Chunk "REMOVED MODULES" MAJOR $
              D.vcat $ map (D.fromString . Module.nameToString) removed
          ]

      chunks =
        addedChunk ++ removedChunk ++ map (changesToChunk localizer) (Map.toList changed)
    in
      D.vcat (header : "" : map chunkToDoc chunks)


data Chunk =
  Chunk
    { _title :: String
    , _magnitude :: Magnitude
    , _details :: D.Doc
    }


chunkToDoc :: Chunk -> D.Doc
chunkToDoc (Chunk title magnitude details) =
  let
    magDoc =
      Diff.magnitudeToString magnitude

    header =
      "----" <+> D.fromString title <+> "-" <+> D.fromString magDoc <+> "----"
  in
    D.vcat
      [ D.dullcyan header
      , ""
      , D.indent 4 details
      , ""
      , ""
      ]


changesToChunk :: L.Localizer -> (N.Name, ModuleChanges) -> Chunk
changesToChunk localizer (name, changes@(ModuleChanges unions aliases values binops)) =
  let
    magnitude =
      Diff.moduleChangeMagnitude changes

    (unionAdd, unionChange, unionRemove) =
      changesToDocTriple (unionToDoc localizer) unions

    (aliasAdd, aliasChange, aliasRemove) =
      changesToDocTriple (aliasToDoc localizer) aliases

    (valueAdd, valueChange, valueRemove) =
      changesToDocTriple (valueToDoc localizer) values

    (binopAdd, binopChange, binopRemove) =
      changesToDocTriple (binopToDoc localizer) binops
  in
    Chunk (N.toString name) magnitude $
      D.vcat $ List.intersperse "" $ Maybe.catMaybes $
        [ changesToDoc "Added" unionAdd aliasAdd valueAdd binopAdd
        , changesToDoc "Removed" unionRemove aliasRemove valueRemove binopRemove
        , changesToDoc "Changed" unionChange aliasChange valueChange binopChange
        ]


changesToDocTriple :: (k -> v -> D.Doc) -> Changes k v -> ([D.Doc], [D.Doc], [D.Doc])
changesToDocTriple entryToDoc (Changes added changed removed) =
  let
    indented (name, value) =
      D.indent 4 (entryToDoc name value)

    diffed (name, (oldValue, newValue)) =
      D.vcat
        [ "  - " <> entryToDoc name oldValue
        , "  + " <> entryToDoc name newValue
        , ""
        ]
  in
    ( map indented (Map.toList added)
    , map diffed   (Map.toList changed)
    , map indented (Map.toList removed)
    )


changesToDoc :: String -> [D.Doc] -> [D.Doc] -> [D.Doc] -> [D.Doc] -> Maybe D.Doc
changesToDoc categoryName unions aliases values binops =
  if null unions && null aliases && null values && null binops then
    Nothing

  else
    Just $ D.vcat $
      D.fromString categoryName <> ":" : unions ++ aliases ++ binops ++ values


unionToDoc :: L.Localizer -> N.Name -> Docs.Union -> D.Doc
unionToDoc localizer name (Docs.Union _ tvars ctors) =
  let
    setup =
      "type" <+> D.fromName name <+> D.hsep (map D.fromName tvars)

    ctorDoc (ctor, tipes) =
      typeDoc localizer (Type.Type ctor tipes)
  in
    D.hang 4 (D.sep (setup : zipWith (<+>) ("=" : repeat "|") (map ctorDoc ctors)))


aliasToDoc :: L.Localizer -> N.Name -> Docs.Alias -> D.Doc
aliasToDoc localizer name (Docs.Alias _ tvars tipe) =
  let
    declaration =
      "type" <+> "alias" <+> D.hsep (map D.fromName (name:tvars)) <+> "="
  in
    D.hang 4 (D.sep [ declaration, typeDoc localizer tipe ])


valueToDoc :: L.Localizer -> N.Name -> Docs.Value -> D.Doc
valueToDoc localizer name (Docs.Value _ tipe) =
  D.hang 4 $ D.sep [ D.fromName name <+> ":", typeDoc localizer tipe ]


binopToDoc :: L.Localizer -> N.Name -> Docs.Binop -> D.Doc
binopToDoc localizer name (Docs.Binop _ tipe associativity (Docs.Precedence n)) =
    "(" <> D.fromName name <> ")" <+> ":" <+> typeDoc localizer tipe <> D.black details
  where
    details =
      "    (" <> D.fromName assoc <> "/" <> D.fromInt n <> ")"

    assoc =
      case associativity of
        Docs.Left  -> "left"
        Docs.Non   -> "non"
        Docs.Right -> "right"


typeDoc :: L.Localizer -> Type.Type -> D.Doc
typeDoc localizer tipe =
  Type.toDoc localizer Type.None tipe
