module Type.Constrain.Declaration where

import Control.Monad
import Control.Applicative ((<$>))

import qualified Data.Map as Map

import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Environment as Env

import SourceSyntax.Declaration
import qualified SourceSyntax.Everything as Src
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
                body = Src.none . Src.Data ctor $ map (Src.none . Src.Var) vars
            in  [ Src.TypeAnnotation ctor $ foldr Type.Lambda tbody tipes
                , Src.Def (Src.PVar ctor) $ buildFunction body vars
                ]

    TypeAlias name tvars tipe@(Type.Record fields ext) ->
        [ Src.TypeAnnotation name $ foldr Type.Lambda tipe args
        , Src.Def (Src.PVar name) $ buildFunction record vars ]
      where
        args = case ext of
                 Type.EmptyRecord -> map snd fields
                 _ -> map snd fields ++ [ext]

        var = Src.none . Src.Var
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = Src.none $ case ext of
                              Type.EmptyRecord -> Src.Record efields
                              _ -> Src.Modify (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    TypeAlias _ _ _ -> []

    ImportEvent _ expr@(Src.L s _) name tipe ->
        [ Src.TypeAnnotation name tipe
        , Src.Def (Src.PVar name) (Src.L s $ Src.App (Src.L s $ Src.Var "constant") expr) ]

    ExportEvent _ name tipe ->
        [ Src.TypeAnnotation name tipe ]

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show n) [1..]

buildFunction body vars =
    foldr (\p e -> Src.none (Src.Lambda p e)) body (map Src.PVar vars)