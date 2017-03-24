module Docs.Centralize (centralize) where

import Control.Arrow (second)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import qualified AST.Declaration as D
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Reporting.Annotation as A



-- CENTRALIZE DOCUMENTATION


centralize :: D.Canonical -> Text -> Docs.Centralized
centralize (D.Decls defs unions aliases infixes) comment =
  let
    infixDict =
      Map.fromList (map infixToEntry infixes)
  in
    Docs.Centralized
      comment
      (Map.fromList (Maybe.mapMaybe (defToEntry infixDict) defs))
      (Map.fromList (map aliasToEntry aliases))
      (Map.fromList (map unionToEntry unions))



-- CONVERSION TO DOC ENTRIES


defToEntry
  :: Map.Map Text (Text, Int)
  -> A.Commented Canonical.Def
  -> Maybe (Text, A.Located Docs.RawValue)
defToEntry infixDict (A.A (_, maybeComment) (Canonical.Def _ pattern _ maybeType)) =
  case pattern of
    A.A subregion (Pattern.Var name) ->
      let
        value =
          Docs.Value
            maybeComment
            (fmap (Extract.extract . A.drop) maybeType)
            (Map.lookup name infixDict)
      in
        Just (name, A.A subregion value)

    _ ->
      Nothing


unionToEntry :: A.Commented (D.Union Type.Canonical) -> (Text, A.Located Docs.RawUnion)
unionToEntry (A.A (region, maybeComment) (D.Type name args ctors)) =
  let
    ctors' =
      map (second (map Extract.extract)) ctors
  in
    (name, A.A region (Docs.Union maybeComment args ctors'))


aliasToEntry :: A.Commented (D.Alias Type.Canonical) -> (Text, A.Located Docs.RawAlias)
aliasToEntry (A.A (region, maybeComment) (D.Type name args tipe)) =
  ( name
  , A.A region (Docs.Alias maybeComment args (Extract.extract tipe))
  )


infixToEntry :: D.Infix -> (Text, (Text, Int))
infixToEntry (D.Infix name assoc precedence) =
  let
    fixity =
      (D.assocToText assoc, precedence)
  in
    ( name, fixity )
