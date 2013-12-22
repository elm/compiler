module Type.Constrain.Declaration where

import Control.Monad
import Control.Applicative ((<$>))

import qualified Data.Map as Map

import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Environment as Env

import SourceSyntax.Declaration
import qualified SourceSyntax.Expression as Src
import qualified SourceSyntax.Location as L
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as Type

toExpr :: [Declaration t v] -> [Src.Def t v]
toExpr = concatMap toDefs

toDefs :: Declaration t v -> [Src.Def t v]
toDefs decl =
  case decl of
    Definition def -> [def]

    Datatype name tvars constructors -> concatMap toDefs constructors
      where
        toDefs (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = Type.Data name $ map Type.Var tvars
                body = L.none . Src.Data ctor $ map (L.none . Src.Var) vars
            in  [ Src.TypeAnnotation ctor $ foldr Type.Lambda tbody tipes
                , Src.Def (P.PVar ctor) $ buildFunction body vars
                ]

    TypeAlias name tvars tipe@(Type.Record fields ext) ->
        [ Src.TypeAnnotation name $ foldr Type.Lambda tipe args
        , Src.Def (P.PVar name) $ buildFunction record vars ]
      where
        args = case ext of
                 Type.EmptyRecord -> map snd fields
                 _ -> map snd fields ++ [ext]

        var = L.none . Src.Var
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = case ext of
                   Type.EmptyRecord -> L.none $ Src.Record efields
                   _ -> foldl (\r (f,v) -> L.none $ Src.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    TypeAlias _ _ _ -> []

    ImportEvent _ expr@(L.L s _) name tipe ->
        [ Src.TypeAnnotation name tipe
        , Src.Def (P.PVar name) (L.L s $ Src.App (L.L s $ Src.Var "constant") expr) ]

    ExportEvent _ name tipe ->
        [ Src.TypeAnnotation name tipe ]

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show n) [1..]

buildFunction body@(L.L s _) vars =
    foldr (\p e -> L.L s (Src.Lambda p e)) body (map P.PVar vars)