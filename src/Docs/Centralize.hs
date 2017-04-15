module Docs.Centralize (centralize) where

import Control.Arrow (second)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Helpers as Help
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- CENTRALIZE


centralize :: Decl.Canonical -> Text -> Docs.Centralized
centralize (Decl.Decls defs unions aliases infixes) overview =
  let
    infixDict =
      Map.fromList (map toInfixPair infixes)
  in
    Docs.Docs
      overview
      (Map.fromList (map unionToEntry unions))
      (Map.fromList (map aliasToEntry aliases))
      (Map.fromList (Maybe.mapMaybe (defToEntry infixDict) defs))



-- ENTRY


type Entry a =
  (Text, Docs.Raw a)


makeEntry :: Text -> (R.Region, Maybe Text) -> details -> Entry details
makeEntry name (region, maybeComment) details =
  ( name, A.A region (Docs.Entry maybeComment details) )



-- UNIONS


unionToEntry :: A.Commented (Decl.Union Type.Canonical) -> Entry Docs.Union
unionToEntry (A.A ann (Decl.Type name args ctors)) =
  makeEntry name ann $ Docs.Union args $
    map (second (map Extract.extract)) ctors



-- ALIASES


aliasToEntry :: A.Commented (Decl.Alias Type.Canonical) -> Entry Docs.Alias
aliasToEntry (A.A ann (Decl.Type name args tipe)) =
  makeEntry name ann $ Docs.Alias args $ Extract.extract tipe



-- VALUES


defToEntry :: Map.Map Text (Decl.Assoc, Int) -> A.Commented Canonical.Def -> Maybe (Entry Docs.RawValue)
defToEntry infixDict (A.A (_, maybeComment) definition) =
  let
    (Canonical.Def _ (A.A region pattern) _ maybeType) =
      definition

    tipe =
      fmap (Extract.extract . A.drop) maybeType
  in
    case pattern of
      Pattern.Var name ->
        Just $ makeEntry name (region, maybeComment) $
          case Map.lookup name infixDict of
            Just (assoc, prec) ->
              Docs.Infix tipe assoc prec

            Nothing ->
              if Help.isOp name then
                Docs.Infix tipe Decl.defaultAssociativity Decl.defaultPrecedence
              else
                Docs.Value tipe

      _ ->
        Nothing


toInfixPair :: Decl.Infix -> ( Text, (Decl.Assoc, Int) )
toInfixPair (Decl.Infix name assoc precedence) =
  ( name, (assoc, precedence) )
