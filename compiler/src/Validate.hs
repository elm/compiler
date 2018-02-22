{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Validate
  ( validate
  )
  where


import Control.Applicative (liftA2)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- VALIDATE


type Result i w a =
  Result.Result i w Error.Error a


validate :: Src.Module [Src.Decl] -> Result i w Valid.Module
validate (Src.Module header imports srcDecls) =
  do  (Stuff decls unions aliases binops ports docs) <- validateDecls srcDecls
      case header of
        Nothing ->
          return $ Valid.defaultModule docs imports decls unions aliases binops

        Just (Src.Header name srcEffects exports overview) ->
          do  effects <- validateEffects srcEffects ports
              return $ Valid.Module name overview docs exports imports decls unions aliases binops effects



-- VALIDATE DECLARATIONS


data Stuff =
  Stuff
    { _decls :: [A.Located Valid.Decl]
    , _unions :: [Valid.Union]
    , _aliases :: [Valid.Alias]
    , _binops :: [Valid.Binop]
    , _ports :: [Valid.Port]
    , _docs :: Map.Map N.Name Text
    }


validateDecls :: [Src.Decl] -> Result i w Stuff
validateDecls decls =
  vsdHelp decls (Stuff [] [] [] [] [] Map.empty)


vsdHelp :: [Src.Decl] -> Stuff -> Result i w Stuff
vsdHelp decls stuff =
  case decls of
    [] ->
      Result.ok stuff

    A.At region decl : otherDecls ->
      case decl of
        Src.Union name tvars ctors ->
          do  let union = Valid.Union region name tvars ctors
              vsdHelp otherDecls $ stuff { _unions = union : _unions stuff }

        Src.Alias name tvars tipe ->
          do  let alias = Valid.Alias region name tvars tipe
              vsdHelp otherDecls $ stuff { _aliases = alias : _aliases stuff }

        Src.Binop op assoc prec val ->
          do  let binop = Valid.Binop op assoc prec val
              vsdHelp otherDecls $ stuff { _binops = binop : _binops stuff }

        Src.Port name tipe ->
          do  let port_ = Valid.Port name tipe
              vsdHelp otherDecls $ stuff { _ports = port_ : _ports stuff }

        Src.Docs docs ->
          validateDocs region docs otherDecls stuff

        Src.Annotation (A.At _ name) tipe ->
          validateAnnotation region name tipe otherDecls stuff

        Src.Definition name args body ->
          do  validBody <- expression body
              let validDecl = A.At region (Valid.Decl name args validBody Nothing)
              vsdHelp otherDecls $ stuff { _decls = validDecl : _decls stuff }



-- VALIDATE DOCS


validateDocs :: R.Region -> Text -> [Src.Decl] -> Stuff -> Result i w Stuff
validateDocs region docs decls stuff =
  case decls of
    [] ->
      Result.throw (Error.CommentOnNothing region)

    A.At _ decl : _ ->
      do  name <- getNameForDocs region decl
          vsdHelp decls $ stuff { _docs = Map.insert name docs (_docs stuff) }


getNameForDocs :: R.Region -> Src.Decl_ -> Result i w N.Name
getNameForDocs region decl =
  case decl of
    Src.Union (A.At _ name) _ _ ->
      Result.ok name

    Src.Alias (A.At _ name) _ _ ->
      Result.ok name

    Src.Binop name _ _ _ ->
      Result.ok name

    Src.Port (A.At _ name) _ ->
      Result.ok name

    Src.Docs _ ->
      Result.throw (Error.CommentOnNothing region)

    Src.Annotation (A.At _ name) _ ->
      Result.ok name

    Src.Definition (A.At _ name) _ _ ->
      Result.ok name



-- VALIDATE ANNOTATION


validateAnnotation :: R.Region -> N.Name -> Src.Type -> [Src.Decl] -> Stuff -> Result i w Stuff
validateAnnotation annRegion annotationName tipe decls stuff =
  case decls of
    [] ->
      Result.throw (Error.TypeWithoutDefinition annRegion annotationName)

    A.At defRegion decl : otherDecls ->
      case decl of
        Src.Definition name@(A.At _ definitionName) args body ->
          if annotationName == definitionName then
            do  validBody <- expression body
                let region = R.merge annRegion defRegion
                let validDecl = A.At region (Valid.Decl name args validBody (Just tipe))
                vsdHelp otherDecls $ stuff { _decls = validDecl : _decls stuff }

          else
            Result.throw (Error.TypeWithBadDefinition annRegion annotationName definitionName)

        _ ->
          Result.throw (Error.TypeWithoutDefinition annRegion annotationName)



-- VALIDATE EXPRESSIONS


expression :: Src.Expr -> Result i w Valid.Expr
expression (A.At region sourceExpression) =
  A.At region <$>
  case sourceExpression of
    Src.Var varType name ->
        pure (Valid.Var varType name)

    Src.VarQual varType home name ->
        pure (Valid.VarQual varType home name)

    Src.Lambda pattern body ->
        Valid.Lambda pattern <$> expression body

    Src.Op name ->
        pure (Valid.Op name)

    Src.Negate expr ->
        Valid.Negate <$> expression expr

    Src.Binops ops lastExpr ->
        Valid.Binops
          <$> traverse (\(expr,op) -> (,) <$> expression expr <*> pure op) ops
          <*> expression lastExpr

    Src.Case expr branches ->
        Valid.Case
          <$> expression expr
          <*> traverse (traverse expression) branches

    Src.Str str ->
        pure (Valid.Str str)

    Src.Chr chr ->
        pure (Valid.Chr chr)

    Src.Int int ->
        pure (Valid.Int int)

    Src.Float float ->
        pure (Valid.Float float)

    Src.List exprs ->
        Valid.List <$> traverse expression exprs

    Src.Call func args ->
        Valid.Call
          <$> expression func
          <*> traverse expression args

    Src.If branches finally ->
        Valid.If
          <$> traverse both branches
          <*> expression finally

    Src.Accessor field ->
        pure (Valid.Accessor field)

    Src.Access record field ->
        Valid.Access
          <$> expression record
          <*> pure field

    Src.Update record fields ->
        Valid.Update record <$> traverse (traverse expression) fields

    Src.Record fields ->
        Valid.Record <$> traverse (traverse expression) fields

    Src.Unit ->
        pure Valid.Unit

    Src.Tuple a b cs ->
        Valid.Tuple
          <$> expression a
          <*> expression b
          <*> traverse expression cs

    Src.Let defs body ->
        Valid.Let
          <$> definitions defs
          <*> expression body

    Src.Shader uid src gltipe ->
        pure (Valid.Shader uid src gltipe)


both :: (Src.Expr, Src.Expr) -> Result i w (Valid.Expr, Valid.Expr)
both (a, b) =
  liftA2 (,) (expression a) (expression b)



-- VALIDATE DEFINITIONS


definitions :: [A.Located Src.Def] -> Result i w [Valid.Def]
definitions sourceDefs =
  case sourceDefs of
    [] ->
      return []

    A.At region (Src.Destruct pattern expr) : otherDefs ->
      (:)
        <$> validateDestruct region pattern expr
        <*> definitions otherDefs

    A.At region (Src.Define name args expr) : otherDefs ->
      (:)
        <$> validateDefinition region name args expr Nothing
        <*> definitions otherDefs

    A.At annRegion (Src.Annotate annotationName tipe) : otherDefs ->
      case otherDefs of
        A.At defRegion (Src.Define name@(A.At _ definitionName) args expr) : otherOtherDefs
          | definitionName == annotationName ->
              (:)
                <$> validateDefinition (R.merge annRegion defRegion) name args expr (Just tipe)
                <*> definitions otherOtherDefs

        _ ->
          Result.throw (Error.TypeWithoutDefinition annRegion annotationName)


validateDefinition :: R.Region -> A.Located N.Name -> [Src.Pattern] -> Src.Expr -> Maybe Src.Type -> Result i w Valid.Def
validateDefinition region name args expr maybeType =
  do  validExpr <- expression expr
      return $ Valid.Define region name args validExpr maybeType


validateDestruct :: R.Region -> Src.Pattern -> Src.Expr -> Result i w Valid.Def
validateDestruct region pattern expr =
  Valid.Destruct region pattern <$> expression expr



-- VALIDATE EFFECTS


validateEffects :: Src.Effects -> [Valid.Port] -> Result i w Valid.Effects
validateEffects effects ports =
  case effects of
    Src.NoEffects ->
      do  noPorts ports
          return Valid.NoEffects

    Src.Ports _ ->
      return (Valid.Ports ports)

    Src.Manager region manager ->
      do  noPorts ports
          return $ Valid.Manager region $
            case manager of
              Src.Cmd cmd ->
                Valid.Cmd cmd

              Src.Sub sub ->
                Valid.Sub sub

              Src.Fx cmd sub ->
                Valid.Fx cmd sub


noPorts :: [Valid.Port] -> Result i w ()
noPorts ports =
  forM_ ports $ \(Valid.Port (A.At region name) _) ->
    Result.throw (Error.UnexpectedPort region name)
