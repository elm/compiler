{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Diff
  ( Args(..)
  , run
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE

import qualified BackgroundWriter as BW
import qualified Build
import Deps.Diff (PackageChanges(..), ModuleChanges(..), Changes(..))
import qualified Deps.Diff as DD
import qualified Deps.Registry as Registry
import qualified Elm.Compiler.Type as Type
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Http
import qualified Reporting
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task
import qualified Stuff



-- RUN


data Args
  = CodeVsLatest
  | CodeVsExactly V.Version
  | LocalInquiry V.Version V.Version
  | GlobalInquiry Pkg.Name V.Version V.Version


run :: Args -> () -> IO ()
run args () =
  Reporting.attempt Exit.diffToReport $
    Task.run $
      do  env <- getEnv
          diff env args



-- ENVIRONMENT


data Env =
  Env
    { _maybeRoot :: Maybe FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: Http.Manager
    , _registry :: Registry.Registry
    }


getEnv :: Task Env
getEnv =
  do  maybeRoot <- Task.io $ Stuff.findRoot
      cache     <- Task.io $ Stuff.getPackageCache
      manager   <- Task.io $ Http.getManager
      registry  <- Task.eio Exit.DiffMustHaveLatestRegistry $ Registry.latest manager cache
      return (Env maybeRoot cache manager registry)



-- DIFF


type Task a =
  Task.Task Exit.Diff a


diff :: Env -> Args -> Task ()
diff env@(Env _ _ _ registry) args =
  case args of
    GlobalInquiry name v1 v2 ->
      case Registry.getVersions' name registry of
        Right vsns ->
          do  oldDocs <- getDocs env name vsns (min v1 v2)
              newDocs <- getDocs env name vsns (max v1 v2)
              writeDiff oldDocs newDocs

        Left suggestions ->
          Task.throw $ Exit.DiffUnknownPackage name suggestions

    LocalInquiry v1 v2 ->
      do  (name, vsns) <- readOutline env
          oldDocs <- getDocs env name vsns (min v1 v2)
          newDocs <- getDocs env name vsns (max v1 v2)
          writeDiff oldDocs newDocs

    CodeVsLatest ->
      do  (name, vsns) <- readOutline env
          oldDocs <- getLatestDocs env name vsns
          newDocs <- generateDocs env
          writeDiff oldDocs newDocs

    CodeVsExactly version ->
      do  (name, vsns) <- readOutline env
          oldDocs <- getDocs env name vsns version
          newDocs <- generateDocs env
          writeDiff oldDocs newDocs



-- GET DOCS


getDocs :: Env -> Pkg.Name -> Registry.KnownVersions -> V.Version -> Task Docs.Documentation
getDocs (Env _ cache manager _) name (Registry.KnownVersions latest previous) version =
  if latest == version || elem version previous
  then Task.eio (Exit.DiffDocsProblem version) $ DD.getDocs cache manager name version
  else Task.throw $ Exit.DiffUnknownVersion name version (latest:previous)


getLatestDocs :: Env -> Pkg.Name -> Registry.KnownVersions -> Task Docs.Documentation
getLatestDocs (Env _ cache manager _) name (Registry.KnownVersions latest _) =
  Task.eio (Exit.DiffDocsProblem latest) $ DD.getDocs cache manager name latest



-- READ OUTLINE


readOutline :: Env -> Task (Pkg.Name, Registry.KnownVersions)
readOutline (Env maybeRoot _ _ registry) =
  case maybeRoot of
    Nothing ->
      Task.throw $ Exit.DiffNoOutline

    Just root ->
      do  result <- Task.io $ Outline.read root
          case result of
            Left err ->
              Task.throw $ Exit.DiffBadOutline err

            Right outline ->
              case outline of
                Outline.App _ ->
                  Task.throw $ Exit.DiffApplication

                Outline.Pkg (Outline.PkgOutline pkg _ _ _ _ _ _ _) ->
                  case Registry.getVersions pkg registry of
                    Just vsns -> return (pkg, vsns)
                    Nothing   -> Task.throw Exit.DiffUnpublished



-- GENERATE DOCS


generateDocs :: Env -> Task Docs.Documentation
generateDocs (Env maybeRoot _ _ _) =
  case maybeRoot of
    Nothing ->
      Task.throw $ Exit.DiffNoOutline

    Just root ->
      do  details <-
            Task.eio Exit.DiffBadDetails $ BW.withScope $ \scope ->
              Details.load Reporting.silent scope root

          case Details._outline details of
            Details.ValidApp _ ->
              Task.throw $ Exit.DiffApplication

            Details.ValidPkg _ exposed _ ->
              case exposed of
                [] ->
                  Task.throw $ Exit.DiffNoExposed

                e:es ->
                  Task.eio Exit.DiffBadBuild $
                    Build.fromExposed Reporting.silent root details Build.KeepDocs (NE.List e es)



-- WRITE DIFF


writeDiff :: Docs.Documentation -> Docs.Documentation -> Task ()
writeDiff oldDocs newDocs =
  let
    changes = DD.diff oldDocs newDocs
    localizer = L.fromNames (Map.union oldDocs newDocs)
  in
  Task.io $ Help.toStdout $ toDoc localizer changes <> "\n"



-- TO DOC


toDoc :: L.Localizer -> PackageChanges -> D.Doc
toDoc localizer changes@(PackageChanges added changed removed) =
  if null added && Map.null changed && null removed then
    "No API changes detected, so this is a" <+> D.green "PATCH" <+> "change."
  else
    let
      magDoc =
        D.fromChars (M.toChars (DD.toMagnitude changes))

      header =
        "This is a" <+> D.green magDoc <+> "change."

      addedChunk =
        if null added then [] else
          [ Chunk "ADDED MODULES" M.MINOR $
              D.vcat $ map D.fromName added
          ]

      removedChunk =
        if null removed then [] else
          [ Chunk "REMOVED MODULES" M.MAJOR $
              D.vcat $ map D.fromName removed
          ]

      chunks =
        addedChunk ++ removedChunk ++ map (changesToChunk localizer) (Map.toList changed)
    in
      D.vcat (header : "" : map chunkToDoc chunks)


data Chunk =
  Chunk
    { _title :: String
    , _magnitude :: M.Magnitude
    , _details :: D.Doc
    }


chunkToDoc :: Chunk -> D.Doc
chunkToDoc (Chunk title magnitude details) =
  let
    header =
      "----" <+> D.fromChars title <+> "-" <+> D.fromChars (M.toChars magnitude) <+> "----"
  in
    D.vcat
      [ D.dullcyan header
      , ""
      , D.indent 4 details
      , ""
      , ""
      ]


changesToChunk :: L.Localizer -> (Name.Name, ModuleChanges) -> Chunk
changesToChunk localizer (name, changes@(ModuleChanges unions aliases values binops)) =
  let
    magnitude =
      DD.moduleChangeMagnitude changes

    (unionAdd, unionChange, unionRemove) =
      changesToDocTriple (unionToDoc localizer) unions

    (aliasAdd, aliasChange, aliasRemove) =
      changesToDocTriple (aliasToDoc localizer) aliases

    (valueAdd, valueChange, valueRemove) =
      changesToDocTriple (valueToDoc localizer) values

    (binopAdd, binopChange, binopRemove) =
      changesToDocTriple (binopToDoc localizer) binops
  in
    Chunk (Name.toChars name) magnitude $
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
      D.fromChars categoryName <> ":" : unions ++ aliases ++ binops ++ values


unionToDoc :: L.Localizer -> Name.Name -> Docs.Union -> D.Doc
unionToDoc localizer name (Docs.Union _ tvars ctors) =
  let
    setup =
      "type" <+> D.fromName name <+> D.hsep (map D.fromName tvars)

    ctorDoc (ctor, tipes) =
      typeDoc localizer (Type.Type ctor tipes)
  in
    D.hang 4 (D.sep (setup : zipWith (<+>) ("=" : repeat "|") (map ctorDoc ctors)))


aliasToDoc :: L.Localizer -> Name.Name -> Docs.Alias -> D.Doc
aliasToDoc localizer name (Docs.Alias _ tvars tipe) =
  let
    declaration =
      "type" <+> "alias" <+> D.hsep (map D.fromName (name:tvars)) <+> "="
  in
    D.hang 4 (D.sep [ declaration, typeDoc localizer tipe ])


valueToDoc :: L.Localizer -> Name.Name -> Docs.Value -> D.Doc
valueToDoc localizer name (Docs.Value _ tipe) =
  D.hang 4 $ D.sep [ D.fromName name <+> ":", typeDoc localizer tipe ]


binopToDoc :: L.Localizer -> Name.Name -> Docs.Binop -> D.Doc
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
