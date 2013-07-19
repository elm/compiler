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
            let vars = take (length tipes) $ map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show n) [1..]
                loc = Src.none
                body = loc . Src.Data ctor $ map (loc . Src.Var) vars
            in  [ Src.TypeAnnotation ctor $
                      foldr Type.Lambda (Type.Data name $ map Type.Var tvars) tipes
                , Src.Def (Src.PVar ctor) $
                      foldr (\p e -> loc $ Src.Lambda p e) body (map Src.PVar vars)
                ]

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    TypeAlias _ _ _ -> []

    ImportEvent _ expr@(Src.L a b _) name tipe ->
        [ Src.TypeAnnotation name tipe
        , Src.Def (Src.PVar name) (Src.L a b $ Src.App (Src.L a b $ Src.Var "constant") expr) ]

    ExportEvent _ name tipe ->
        [ Src.TypeAnnotation name tipe ]

    -- no constraints are needed for fixity declarations
    Fixity _ _ _ -> []
