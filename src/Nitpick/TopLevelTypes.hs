{-# OPTIONS_GHC -Wall #-}
module Nitpick.TopLevelTypes (topLevelTypes) where

import Prelude hiding (maybe)
import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as Decl
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


topLevelTypes
    :: Map.Map String Type.Canonical
    -> [Decl.ValidDecl]
    -> Result.Result Warning.Warning Error.Error ()
topLevelTypes typeEnv validDecls =
  do  F.traverse_ (warnMissingAnnotation typeEnv) validDecls
      checkMainType typeEnv validDecls


-- MISSING ANNOTATIONS

warnMissingAnnotation
    :: Map.Map String Type.Canonical
    -> Decl.ValidDecl
    -> Result.Result Warning.Warning Error.Error ()
warnMissingAnnotation typeEnv (A.A (region,_) decl) =
  case decl of
    Decl.Definition (Valid.Definition (A.A _ (P.Var name)) _ Nothing) ->
        case Map.lookup name typeEnv of
          Nothing ->
              return ()
          Just tipe ->
              Result.warn region (Warning.MissingTypeAnnotation name tipe)
    _ ->
        return ()


-- MAIN TYPE

checkMainType
    :: Map.Map String Type.Canonical
    -> [Decl.ValidDecl]
    -> Result.Result w Error.Error ()
checkMainType typeEnv decls =
    case decls of
      A.A (region,_) (Decl.Definition (Valid.Definition (A.A _ (P.Var "main")) _ _)) : _ ->
          case Map.lookup "main" typeEnv of
            Nothing ->
                return ()

            Just typeOfMain ->
                let tipe = Type.deepDealias typeOfMain
                in
                    if tipe `elem` validMainTypes
                      then return ()
                      else Result.throw region (Error.BadMain typeOfMain)

      _ : remainingDecls ->
          checkMainType typeEnv remainingDecls

      [] ->
          return ()


validMainTypes :: [Type.Canonical]
validMainTypes =
    [ element
    , html
    , signal element
    , signal html
    ]
  where
    fromModule :: [String] -> String -> Type.Canonical
    fromModule home name =
      Type.Type (Var.fromModule home name)

    html =
        fromModule ["VirtualDom"] "Node"

    signal tipe =
        Type.App (fromModule ["Signal"] "Signal") [ tipe ]

    element =
      let builtin name =
            Type.Type (Var.builtin name)

          maybe tipe =
            Type.App (fromModule ["Maybe"] "Maybe") [ tipe ]
      in
        Type.Record
          [ ("element", fromModule ["Graphics","Element"] "ElementPrim")
          , ("props",
              Type.Record
                [ ("click"  , builtin "_Tuple0")
                , ("color"  , maybe (fromModule ["Color"] "Color"))
                , ("height" , builtin "Int")
                , ("hover"  , builtin "_Tuple0")
                , ("href"   , builtin "String")
                , ("id"     , builtin "Int")
                , ("opacity", builtin "Float")
                , ("tag"    , builtin "String")
                , ("width"  , builtin "Int")
                ]
                Nothing
            )
          ]
          Nothing
