{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Declaration where

import qualified SourceSyntax.Annotation as A
import qualified SourceSyntax.Declaration as D
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Type as T

toExpr :: [D.Declaration] -> [E.Def]
toExpr = concatMap toDefs

toDefs :: D.Declaration -> [E.Def]
toDefs decl =
  case decl of
    D.Definition def -> [def]

    D.Datatype name tvars constructors -> concatMap toDefs' constructors
      where
        toDefs' (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = T.Data name $ map T.Var tvars
                body = A.none . E.Data ctor $ map (A.none . E.rawVar) vars
            in  [ definition ctor (buildFunction body vars) (foldr T.Lambda tbody tipes) ]

    D.TypeAlias name _ tipe@(T.Record fields ext) ->
        [ definition name (buildFunction record vars) (foldr T.Lambda tipe args) ]
      where
        args = map snd fields ++ maybe [] (\x -> [T.Var x]) ext

        var = A.none . E.rawVar
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record = case ext of
                   Nothing -> A.none $ E.Record efields
                   Just _ -> foldl (\r (f,v) -> A.none $ E.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    D.TypeAlias _ _ _ -> []

    D.Port port ->
        case port of
          D.Out name expr@(A.A s _) tipe ->
              [ definition name (A.A s $ E.PortOut name tipe expr) tipe ]
          D.In name tipe ->
              [ definition name (A.none $ E.PortIn name tipe) tipe ]

    -- no constraints are needed for fixity declarations
    D.Fixity _ _ _ -> []


arguments :: [String]
arguments = map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]

buildFunction :: E.Expr -> [String] -> E.Expr
buildFunction body@(A.A s _) vars =
    foldr (\p e -> A.A s (E.Lambda p e)) body (map P.Var vars)

definition :: String -> E.Expr -> T.Type -> E.Def
definition name expr tipe = E.Definition (P.Var name) expr (Just tipe)
