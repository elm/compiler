{-# OPTIONS_GHC -Wall #-}

{-| This module contains checks to be run *after* type inference has completed
successfully. At that point we still need to do occurs checks and ensure that
`main` has an acceptable type.
-}
module Type.ExtraChecks (effectTypes, occurs) where

import Prelude hiding (maybe)
import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Traversable as Traverse
import qualified Data.UnionFind.IO as UF
import Text.PrettyPrint as P

import qualified AST.Annotation as A
import qualified AST.PrettyPrint as PP
import qualified AST.Type as ST
import qualified AST.Variable as Var
import qualified Type.Hint as Hint
import qualified Type.Type as TT
import qualified Type.State as TS


-- EFFECT TYPE CHECKS

effectTypes :: TS.Env -> ErrorT [P.Doc] IO (Map.Map String ST.CanonicalType)
effectTypes environment =
  do  environment' <- liftIO $ Traverse.traverse TT.toSrcType environment
      mainCheck environment'
      return environment'


-- MAIN TYPE

mainCheck
    :: (Monad m)
    => Map.Map String ST.CanonicalType
    -> ErrorT [P.Doc] m ()
mainCheck env =
  case Map.lookup "main" env of
    Nothing ->
        return ()

    Just typeOfMain ->
        let tipe = ST.deepDealias typeOfMain
        in
            if tipe `elem` validMainTypes
              then return ()
              else throwError [ badMainMessage typeOfMain ]


validMainTypes :: [ST.CanonicalType]
validMainTypes =
    [ element
    , html
    , signal element
    , signal html
    ]
  where
    fromModule :: [String] -> String -> ST.CanonicalType
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


badMainMessage :: ST.CanonicalType -> P.Doc
badMainMessage typeOfMain =
  P.vcat
    [ P.text "Type Error: 'main' must have one of the following types:"
    , P.text " "
    , P.text "    Element, Html, Signal Element, Signal Html"
    , P.text " "
    , P.text "Instead 'main' has type:\n"
    , P.nest 4 (PP.pretty typeOfMain)
    , P.text " "
    ]


-- INFINITE TYPES

occurs :: (String, TT.Variable) -> StateT TS.SolverState IO ()
occurs (name, variable) =
  do  vars <- liftIO $ infiniteVars [] variable
      case vars of
        [] ->
          return ()

        var : _ ->
          do  desc <- liftIO $ UF.descriptor var
              case TT.structure desc of
                Nothing ->
                  TS.addHint (P.text msg)

                Just _ ->
                  do  liftIO $ UF.setDescriptor var (desc { TT.structure = Nothing })
                      var' <- liftIO $ UF.fresh desc
                      hint <- liftIO $ Hint.create (A.None (P.text name)) (Just msg) var var'
                      TS.addHint hint
  where
    msg = "Infinite types are not allowed"

    infiniteVars :: [TT.Variable] -> TT.Variable -> IO [TT.Variable]
    infiniteVars seen var =
        let go = infiniteVars (var:seen) in
        if var `elem` seen
        then return [var]
        else do
          desc <- UF.descriptor var
          case TT.structure desc of
            Nothing -> return []
            Just struct ->
                case struct of
                  TT.App1 a b ->
                      (++) <$> go a <*> go b

                  TT.Fun1 a b ->
                      (++) <$> go a <*> go b

                  TT.Var1 a ->
                      go a

                  TT.EmptyRecord1 ->
                      return []

                  TT.Record1 fields ext ->
                      concat <$> mapM go (ext : concat (Map.elems fields))
