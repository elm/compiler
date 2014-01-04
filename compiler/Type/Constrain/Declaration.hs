{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Declaration where

import SourceSyntax.Declaration
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Location as L
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as Type

toExpr :: [Declaration] -> [E.Def]
toExpr = concatMap toDefs

toDefs :: Declaration -> [E.Def]
toDefs decl =
  case decl of
    Definition def -> [def]

    Datatype name tvars constructors _ -> concatMap toDefs' constructors
      where
        toDefs' (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = Type.Data name $ map Type.Var tvars
                body = L.none . E.Data ctor $ map (L.none . E.Var) vars
            in  [ definition ctor (buildFunction body vars) (foldr Type.Lambda tbody tipes) ]

    TypeAlias name _ tipe@(Type.Record fields ext) _ ->
        [ definition name (buildFunction record vars) (foldr Type.Lambda tipe args) ]
      where
        args = case ext of
                 Type.EmptyRecord -> map snd fields
                 _ -> map snd fields ++ [ext]

        var = L.none . E.Var
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = case ext of
                   Type.EmptyRecord -> L.none $ E.Record efields
                   _ -> foldl (\r (f,v) -> L.none $ E.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    -- TODO: with the ability to derive code, you may need to generate stuff!
    TypeAlias _ _ _ _ -> []

    Port port ->
        case port of
          Send name expr tipe -> [ definition name expr tipe ]
          Recv _ _ _ -> -- [ definition name ]
              error "not sure how to generate constraints for recv yet"

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]

buildFunction :: E.LExpr -> [String] -> E.LExpr
buildFunction body@(L.L s _) vars =
    foldr (\p e -> L.L s (E.Lambda p e)) body (map P.PVar vars)

definition :: String -> E.LExpr -> Type.Type -> E.Def
definition name expr tipe = E.Definition (P.PVar name) expr (Just tipe)