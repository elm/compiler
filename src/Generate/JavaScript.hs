{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript (generate) where

import Control.Monad (foldM)
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Builder as BS

import qualified AST.Effects as Effects
import qualified AST.Expression.Optimized as Opt
import qualified AST.Kernel as Kernel
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Elm.Compiler.Objects.Internal as Obj
import qualified Generate.JavaScript.Builder as JsBuilder
import qualified Generate.JavaScript.Effects as JsEffects
import qualified Generate.JavaScript.Expression as JsExpr
import qualified Generate.JavaScript.Variable as JS



-- GENERATE JAVASCRIPT


generate :: Obj.Graph -> Obj.Roots -> BS.Builder
generate graph roots =
  let
    (State builders _ _ effects) =
      List.foldl' (crawl graph) initialState (Obj.toGlobals roots)

    managers =
      JS.run (JsEffects.generate effects)
  in
    List.foldl' (\rest stmt -> stmt <> rest) managers builders



-- CRAWL STATE


data State =
  State
    { _builders :: [BS.Builder]
    , _seenDecls :: Set.Set Var.Global
    , _seenKernels :: Set.Set ModuleName.Raw
    , _effects :: Map.Map ModuleName.Canonical Effects.ManagerType
    }


initialState :: State
initialState =
  State [] Set.empty Set.empty Map.empty



-- CRAWL


crawl :: Obj.Graph -> State -> Var.Global -> State
crawl graph@(Obj.Graph decls kernels) state@(State _ seenDecls seenKernels _) name@(Var.Global home _) =
  if Set.member name seenDecls then
    state

  else if home == virtualDomDebug then
    state

  else
    case Map.lookup name decls of
      Just decl ->
        crawlDecl graph name decl state

      Nothing ->
        let
          kernelModule =
            ModuleName._module home
        in
          if Set.member kernelModule seenKernels then
            state
          else
            case Map.lookup kernelModule kernels of
              Just info ->
                crawlKernel graph kernelModule info state

              Nothing ->
                error (crawlError name)



-- CRAWL DECL


crawlDecl :: Obj.Graph -> Var.Global -> Opt.Decl -> State -> State
crawlDecl graph var@(Var.Global home name) (Opt.Decl direct indirect fx body) state1 =
  let
    state2 =
      state1 { _seenDecls = Set.insert var (_seenDecls state1) }

    (State builders seenDecls seenKernels effects) =
      Set.foldl' (crawl graph) state2 direct

    stmt =
      JS.run (JsExpr.generateDecl home name body)

    state4 =
      State
        { _builders = JsBuilder.stmtToBuilder stmt : builders
        , _seenDecls = seenDecls
        , _seenKernels = seenKernels
        , _effects = maybe id (Map.insert home) fx effects
        }
  in
    Set.foldl' (crawl graph) state4 indirect



-- CRAWL KERNEL


crawlKernel :: Obj.Graph -> ModuleName.Raw -> Kernel.Info -> State -> State
crawlKernel graph name (Kernel.Info imports chunks) state1 =
  let
    state2 =
      state1 { _seenKernels = Set.insert name (_seenKernels state1) }

    (State builders seenDecls seenKernels effects) =
      List.foldl' (crawl graph) state2 (map toCoreGlobal imports)

    builder =
      JS.run (foldM chunkToBuilder mempty chunks)
  in
    State
      { _builders = builder : builders
      , _seenDecls = seenDecls
      , _seenKernels = seenKernels
      , _effects = effects
      }


toCoreGlobal :: ( ModuleName.Raw, Text.Text ) -> Var.Global
toCoreGlobal (home, name) =
  Var.Global (ModuleName.inCore home) name


chunkToBuilder :: BS.Builder -> Kernel.Chunk -> JS.Generator BS.Builder
chunkToBuilder builder chunk =
  case chunk of
    Kernel.JS javascript ->
      return $ BS.byteString javascript <> builder

    Kernel.Var home name ->
      do  expr <- JS.global (Var.Global (ModuleName.inCore home) name)
          return $ JsBuilder.exprToBuilder expr <> builder

    Kernel.Field name ->
      -- TODO generate a smaller field
      return $ Text.encodeUtf8Builder name <> builder

    Kernel.Prod isProd ->
      -- TODO pick based on what compile mode we are in
      if isProd then
        return builder
      else
        return $ "_UNUSED" <> builder



-- CRAWL HELPERS


crawlError :: Var.Global -> String
crawlError (Var.Global (ModuleName.Canonical pkg home) name) =
  Text.unpack $
    "compiler bug manifesting in Generate.JavaScript\n"
    <> "Could not find " <> Pkg.toText pkg <> " " <> ModuleName.toText home <> "." <> name <> "\n"
    <> "Please report at <https://github.com/elm-lang/elm-compiler/issues>\n"
    <> "Try to make an <http://sscce.org/> that demonstrates the issue!"


virtualDomDebug :: ModuleName.Canonical
virtualDomDebug =
  ModuleName.inVirtualDom "VirtualDom.Debug"
