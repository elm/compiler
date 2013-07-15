module Type.Constrain.Declaration where

import Control.Monad
import Control.Applicative ((<$>))

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Environment as Env

import SourceSyntax.Declaration
import qualified SourceSyntax.Location as SL
import qualified SourceSyntax.Literal as SL
import qualified SourceSyntax.Pattern as SP
import qualified SourceSyntax.Expression as SE
import qualified SourceSyntax.Type as ST


toExpr decls =
  SL.none $ SE.Let (concatMap toDefs decls) (SL.none $ SE.Literal (SL.IntNum 42))

toDefs :: Declaration t v -> [SE.Def t v]
toDefs decl =
  case decl of
    Definition def -> [def]

    Datatype name tvars constructors -> map toAnnotation constructors
      where
        toAnnotation (ctor, tipes) =
            SE.TypeAnnotation ctor $
                foldr ST.Lambda (ST.Data name $ map ST.Var tvars) tipes

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    TypeAlias _ _ _ -> []

    ImportEvent _ expr@(SL.L a b _) name tipe ->
        [ SE.TypeAnnotation name tipe
        , SE.Def (SP.PVar name) (SL.L a b $ SE.App (SL.L a b $ SE.Var "constant") expr) ]

    ExportEvent _ name tipe ->
        [ SE.TypeAnnotation name tipe ]

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []

toEqualityDict :: Declaration t v -> Maybe (String, IO T.Type)
toEqualityDict decl =
  case decl of
    Definition _ -> Nothing
    Datatype _ _ _ -> Nothing
    ImportEvent _ _ _ _ -> Nothing
    ExportEvent _ _ _ -> Nothing
    Fixity _ _ _ -> Nothing
    TypeAlias alias tvars tipe -> Just $ (,) alias $ do
        pairs <- forM tvars $ \tname -> (,) tname <$> T.namedVar tname
        TcExpr.instantiateTypeWithContext tipe (Map.fromList pairs)
