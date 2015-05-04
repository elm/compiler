{-# OPTIONS_GHC -Wall #-}

{-| This module contains checks to be run *after* type inference has completed
successfully. At that point we still need to ensure that `main` has an
acceptable type.
-}
module Type.ExtraChecks (mainType) where

import Prelude hiding (maybe)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Map as Map

import qualified AST.Type as ST
import qualified AST.Variable as Var


-- MAIN TYPE

mainType
    :: (Monad m)
    => Map.Map String ST.Canonical
    -> ExceptT ST.Canonical m ()
mainType env =
  case Map.lookup "main" env of
    Nothing ->
        return ()

    Just typeOfMain ->
        let tipe = ST.deepDealias typeOfMain
        in
            if tipe `elem` validMainTypes
              then return ()
              else throwError typeOfMain


validMainTypes :: [ST.Canonical]
validMainTypes =
    [ element
    , html
    , signal element
    , signal html
    ]
  where
    fromModule :: [String] -> String -> ST.Canonical
    fromModule home name =
      ST.Type (Var.fromModule home name)

    html =
        fromModule ["VirtualDom"] "Node"

    signal tipe =
        ST.App (fromModule ["Signal"] "Signal") [ tipe ]

    element =
      let builtin name =
            ST.Type (Var.builtin name)

          maybe tipe =
            ST.App (fromModule ["Maybe"] "Maybe") [ tipe ]
      in
        ST.Record
          [ ("element", fromModule ["Graphics","Element"] "ElementPrim")
          , ("props",
              ST.Record
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
