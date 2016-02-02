module Docs.Centralize (centralize) where

import Control.Arrow (second)
import qualified Data.Map as Map

import qualified AST.Declaration as D
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as P
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Reporting.Annotation as A


-- CENTRALIZE DOCUMENTATION

centralize :: [D.CanonicalDecl] -> String -> Docs.Centralized
centralize decls comment =
  let
    (Raw aliases types valueInfo fixities) =
      toRawDocs decls

    values =
      Map.fromList (map (toValue fixities) (Map.toList valueInfo))
  in
    Docs.Docs comment aliases types values


toValue
    :: Map.Map String (String, Int)
    -> ( String, A.Located (Maybe String, Maybe Type.Type) )
    -> ( String, A.Located (Docs.Value (Maybe Type.Type)) )
toValue fixities (name, A.A region (maybeComment, maybeType)) =
  ( name
  , A.A region (Docs.Value maybeComment maybeType (Map.lookup name fixities))
  )


-- GATHER INFORMATION

data Raw = Raw
    { rawAliases :: Map.Map String (A.Located Docs.Alias)
    , rawTypes :: Map.Map String (A.Located Docs.Union)
    , rawValues :: Map.Map String (A.Located (Maybe String, Maybe Type.Type))
    , rawFixities :: Map.Map String (String, Int)
    }


toRawDocs :: [D.CanonicalDecl] -> Raw
toRawDocs decls =
  foldr addDeclToDocs (Raw Map.empty Map.empty Map.empty Map.empty) decls


addDeclToDocs :: D.CanonicalDecl -> Raw -> Raw
addDeclToDocs (A.A (region,maybeComment) decl) docs =
  case decl of
    D.Definition (Canonical.Definition _ (A.A subregion (P.Var name)) _ maybeType) ->
        let
          info =
            ( maybeComment
            , fmap (Extract.toAliasedType . A.drop) maybeType
            )

          newValues =
            Map.insert name (A.A subregion info) (rawValues docs)
        in
          docs { rawValues = newValues }

    D.Definition _ ->
        docs

    D.Datatype name args ctors ->
        let
          ctors' =
            map (second (map Extract.toAliasedType)) ctors

          union =
            A.A region (Docs.Union maybeComment args ctors')
        in
          docs { rawTypes = Map.insert name union (rawTypes docs) }

    D.TypeAlias name args tipe ->
        let
          alias =
            A.A region (Docs.Alias maybeComment args (Extract.toAliasedType tipe))
        in
          docs { rawAliases = Map.insert name alias (rawAliases docs) }

    D.Port _ ->
        docs

    D.Fixity assoc precedence name ->
        let
          fixity =
            (D.assocToString assoc, precedence)
        in
          docs
          { rawFixities =
              Map.insert name fixity (rawFixities docs)
          }
