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
import Text.PrettyPrint.ANSI.Leijen ((<>), (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Deps.Diff (PackageChanges(..), ModuleChanges(..), Changes(..), Magnitude(..))
import qualified Deps.Diff as Diff
import qualified Deps.Get as Get
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Diff as E
import qualified Reporting.Exit.Help as Help
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
      do  pkgs <- Get.all Get.RequireLatest
          case Get.versions name pkgs of
            Left suggestions ->
              throw $ E.UnknownPackage name suggestions

            Right vsns ->
              do  oldDocs <- getDocs name vsns (min v1 v2)
                  newDocs <- getDocs name vsns (max v1 v2)
                  writeDoc (toDoc (Diff.diff oldDocs newDocs))

    LocalInquiry v1 v2 ->
      do  (_, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (min v1 v2)
          newDocs <- getDocs name vsns (max v1 v2)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))

    CodeVsLatest ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns (maximum vsns)
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))

    CodeVsExactly version ->
      do  (summary, name, vsns) <- getPackageInfo
          oldDocs <- getDocs name vsns version
          newDocs <- Task.silently (Project.generateDocs summary)
          writeDoc (toDoc (Diff.diff oldDocs newDocs))


throw :: E.Exit -> Task.Task a
throw exit =
  Task.throw $ Exit.Diff exit



-- DIFF HELPERS


getDocs :: Pkg.Name -> [Pkg.Version] -> Pkg.Version -> Task.Task Docs.Documentation
getDocs name allVersions version =
  if elem version allVersions then
    Get.docs name version
  else
    throw $ E.UnknownVersion name version allVersions


getPackageInfo :: Task.Task (Summary.Summary, Pkg.Name, [Pkg.Version])
getPackageInfo =
  do  summary <- Project.getRoot
      case Summary._project summary of
        Project.App _ ->
          throw $ E.Application

        Project.Pkg (Project.PkgInfo name _ _ _ _ _ _ _) ->
          do  pkgs <- Get.all Get.RequireLatest
              let vsns = either (const []) id (Get.versions name pkgs)
              return ( summary, name, vsns )



-- WRITE DOC


writeDoc :: P.Doc -> Task.Task ()
writeDoc doc =
  liftIO $ Help.toStdout (doc <> "\n")



-- TO DOC


toDoc :: PackageChanges -> P.Doc
toDoc changes@(PackageChanges added changed removed) =
  if null added && Map.null changed && null removed then
    "No API changes detected, so this is a" <+> P.green "PATCH" <+> "change."
  else
    let
      magDoc =
        P.text (Diff.magnitudeToString (Diff.toMagnitude changes))

      header =
        "This is a" <+> P.green magDoc <+> "change."

      addedChunk =
        if null added then [] else
          [ Chunk "ADDED MODULES" MINOR $
              P.vcat $ map (P.text . Module.nameToString) added
          ]

      removedChunk =
        if null removed then [] else
          [ Chunk "REMOVED MODULES" MAJOR $
              P.vcat $ map (P.text . Module.nameToString) removed
          ]

      chunks =
        addedChunk ++ removedChunk ++ map changesToChunk (Map.toList changed)
    in
      P.vcat (header : "" : map chunkToDoc chunks)


data Chunk =
  Chunk
    { _title :: String
    , _magnitude :: Magnitude
    , _details :: P.Doc
    }


chunkToDoc :: Chunk -> P.Doc
chunkToDoc (Chunk title magnitude details) =
  let
    magDoc =
      Diff.magnitudeToString magnitude

    header =
      "----" <+> P.text title <+> "-" <+> P.text magDoc <+> "----"
  in
    P.vcat
      [ P.dullcyan header
      , ""
      , P.indent 4 details
      , ""
      , ""
      ]


changesToChunk :: (N.Name, ModuleChanges) -> Chunk
changesToChunk (name, changes@(ModuleChanges unions aliases values binops)) =
  let
    magnitude =
      Diff.moduleChangeMagnitude changes

    (unionAdd, unionChange, unionRemove) =
      changesToDocTriple unionToDoc unions

    (aliasAdd, aliasChange, aliasRemove) =
      changesToDocTriple aliasToDoc aliases

    (valueAdd, valueChange, valueRemove) =
      changesToDocTriple valueToDoc values

    (binopAdd, binopChange, binopRemove) =
      changesToDocTriple binopToDoc binops
  in
    Chunk (N.toString name) magnitude $
      P.vcat $ List.intersperse "" $ Maybe.catMaybes $
        [ changesToDoc "Added" unionAdd aliasAdd valueAdd binopAdd
        , changesToDoc "Removed" unionRemove aliasRemove valueRemove binopRemove
        , changesToDoc "Changed" unionChange aliasChange valueChange binopChange
        ]


changesToDocTriple :: (k -> v -> P.Doc) -> Changes k v -> ([P.Doc], [P.Doc], [P.Doc])
changesToDocTriple entryToDoc (Changes added changed removed) =
  let
    indented (name, value) =
      P.indent 4 (entryToDoc name value)

    diffed (name, (oldValue, newValue)) =
      P.vcat
        [ "  - " <> entryToDoc name oldValue
        , "  + " <> entryToDoc name newValue
        , ""
        ]
  in
    ( map indented (Map.toList added)
    , map diffed   (Map.toList changed)
    , map indented (Map.toList removed)
    )


changesToDoc :: String -> [P.Doc] -> [P.Doc] -> [P.Doc] -> [P.Doc] -> Maybe P.Doc
changesToDoc categoryName unions aliases values binops =
  if null unions && null aliases && null values && null binops then
    Nothing

  else
    Just $ P.vcat $
      P.text categoryName <> ":" : unions ++ aliases ++ binops ++ values


unionToDoc :: N.Name -> Docs.Union -> P.Doc
unionToDoc name (Docs.Union _ tvars ctors) =
  let
    setup =
      "type" <+> nameToDoc name <+> P.hsep (map nameToDoc tvars)

    ctorDoc (ctor, tipes) =
      typeDoc (Type.Type ctor tipes)
  in
    P.hang 4 (P.sep (setup : zipWith (<+>) ("=" : repeat "|") (map ctorDoc ctors)))


aliasToDoc :: N.Name -> Docs.Alias -> P.Doc
aliasToDoc name (Docs.Alias _ tvars tipe) =
  let
    declaration =
      "type" <+> "alias" <+> P.hsep (map nameToDoc (name:tvars)) <+> "="
  in
    P.hang 4 (P.sep [ declaration, typeDoc tipe ])


valueToDoc :: N.Name -> Docs.Value -> P.Doc
valueToDoc name (Docs.Value _ tipe) =
  P.hang 4 $ P.sep [ nameToDoc name <+> P.colon, typeDoc tipe ]


binopToDoc :: N.Name -> Docs.Binop -> P.Doc
binopToDoc name (Docs.Binop _ tipe associativity (Docs.Precedence n)) =
    "(" <> nameToDoc name <> ")" <+> P.colon <+> typeDoc tipe <> P.black details
  where
    details =
      "    (" <> nameToDoc assoc <> "/" <> P.int n <> ")"

    assoc =
      case associativity of
        Docs.Left  -> "left"
        Docs.Non   -> "non"
        Docs.Right -> "right"


typeDoc :: Type.Type -> P.Doc
typeDoc tipe =
  Type.toDoc Type.None tipe


nameToDoc :: N.Name -> P.Doc
nameToDoc name =
  P.text (N.toString name)
