{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Type.Extract
  ( extract
  , extractProgram
  )
  where

import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified AST.Variable as Var
import Elm.Compiler.Type (Type(..), Program(Program))



-- EXTRACTION


extract :: T.Canonical -> Type
extract astType =
  fst $ Writer.runWriter $ extractHelp astType


type Deps =
  ( Set.Set Var.Canonical -- aliases
  , Set.Set Var.Canonical -- unions
  )


diff :: Deps -> Deps -> Deps
diff (oldAliases, oldUnions) (newAliases, newUnions) =
  ( Set.difference newAliases oldAliases
  , Set.difference newUnions oldUnions
  )


extractHelp :: T.Canonical -> Writer.Writer Deps Type
extractHelp astType =
  case astType of
    T.Lambda arg result ->
      Lambda <$> extractHelp arg <*> extractHelp result

    T.Var x ->
      return $ Var x

    T.Type var ->
      do  Writer.tell ( Set.empty, Set.singleton var )
          return $ Type $ Var.toString var

    T.App constructor args ->
      App <$> extractHelp constructor <*> traverse extractHelp args

    T.Record fields ext ->
      Record
        <$> traverse (traverse extractHelp) fields
        <*> traverse extractHelp ext

    T.Aliased name args aliasedType ->
      do  Writer.tell ( Set.singleton name, Set.empty )
          _ <- extractHelp (T.dealias args aliasedType)
          App (Type (Var.toString name)) <$> traverse (extractHelp . snd) args



-- EXTRACT MODEL, MSG, AND ANY TRANSITIVE DEPENDENCIES


extractProgram :: Module.Interfaces -> ModuleName.Canonical -> Maybe Program
extractProgram interfaces name =
  do  iface <- Map.lookup name interfaces
      mainType <- Map.lookup "main" (Module.iTypes iface)

      message <-
        case T.deepDealias mainType of
          T.App _program [_flags, _model, msg] -> Just msg
          T.App _node [msg] -> Just msg
          _ -> Nothing

      let (msgType, msgDeps) = Writer.runWriter (extractHelp message)
      let (aliases, unions) = extractTransitive interfaces mempty msgDeps
      Just $ Program msgType aliases unions


type Alias =
  ( String, [String], Type )


type Union =
  ( String, [String], [(String, [Type])] )


extractTransitive :: Module.Interfaces -> Deps -> Deps -> ( [Alias], [Union] )
extractTransitive interfaces oldDeps currentDeps =
  let
    (aliases, unions) =
      diff oldDeps currentDeps
  in
    if Set.null aliases && Set.null unions then
      ( [], [] )

    else
      let
        (result, newDeps) =
          Writer.runWriter $ do
            publicAliases <- mapM (extractAlias interfaces) (Set.toList aliases)
            publicUnions <- mapM (extractUnion interfaces) (Set.toList unions)
            return ( Maybe.catMaybes publicAliases, Maybe.catMaybes publicUnions )

        remainingResult =
          extractTransitive interfaces (mappend oldDeps currentDeps) newDeps
      in
        mappend result remainingResult


extractAlias :: Module.Interfaces -> Var.Canonical -> Writer.Writer Deps (Maybe Alias)
extractAlias interfaces var =
  let
    toAlias (args, tipe) =
      (,,) (Var.toString var) args <$> extractHelp tipe
  in
    traverse toAlias (get Module.iAliases interfaces var)


extractUnion :: Module.Interfaces -> Var.Canonical -> Writer.Writer Deps (Maybe Union)
extractUnion interfaces var =
  let
    toUnion (args, constructors) =
      (,,) (Var.toString var) args
        <$> traverse (traverse (traverse extractHelp)) constructors
  in
    traverse toUnion (get Module.iUnions interfaces var)


get :: (Module.Interface -> Map.Map String a) -> Module.Interfaces -> Var.Canonical -> Maybe a
get getInfo interfaces var =
  do  (home, name) <- getHome var
      iface <- Map.lookup home interfaces
      Map.lookup name (getInfo iface)


getHome :: Var.Canonical -> Maybe (ModuleName.Canonical, String)
getHome (Var.Canonical home name) =
  case home of
    Var.BuiltIn ->
      Nothing

    Var.Module moduleName ->
      Just (moduleName, name)

    Var.TopLevel moduleName ->
      Just (moduleName, name)

    Var.Local ->
      Nothing