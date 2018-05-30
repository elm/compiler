{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript
  ( Output(..)
  , generate
  , generateForRepl
  )
  where


import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as Expr
import qualified Generate.JavaScript.Name as Name
import qualified Generate.JavaScript.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- GENERATE MAINS


data Output
  = None
  | Some N.Name [N.Name] B.Builder


generate :: Mode.Mode -> Opt.Graph -> [ModuleName.Canonical] -> Output
generate mode (Opt.Graph mains graph _fields) roots =
  let
    rootSet = Set.fromList roots
    rootMap = Map.restrictKeys mains rootSet
  in
  case map ModuleName._module (Map.keys rootMap) of
    [] ->
      None

    name:names ->
      let
        state = Map.foldrWithKey (addMain mode graph) emptyState rootMap
        builder = perfNote mode <> stateToBuilder state <> toMainExports mode rootMap
      in
      Some name names builder


addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")


perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ _ ->
      ""

    Mode.Dev _ Nothing ->
      "console.warn('Compiled in DEV mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"

    Mode.Dev _ (Just _) ->
      "console.warn('Compiled in DEBUG mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"



-- GENERATE FOR REPL


generateForRepl :: Bool -> L.Localizer -> Opt.Graph -> I.Interface -> ModuleName.Canonical -> N.Name -> B.Builder
generateForRepl ansi localizer (Opt.Graph _ graph _) iface home name =
  let
    mode = Mode.dev Mode.Client
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug "toString")
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  stateToBuilder evalState
  <>
  print ansi localizer home name (I._types iface ! name)


print :: Bool -> L.Localizer -> ModuleName.Canonical -> N.Name -> Can.Annotation -> B.Builder
print ansi localizer home name (Can.Forall _ tipe) =
  let
    value = Name.toBuilder (Name.fromGlobal home name)
    toString = Name.toBuilder (Name.fromKernel N.debug "toAnsiString")
    tipeDoc = RT.canToDoc localizer RT.None tipe
    bool = if ansi then "true" else "false"
  in
    "var _value = " <> toString <> "(" <> bool <> ", " <> value <> ");\n\
    \var _type = " <> B.stringUtf8 (show (D.toString tipeDoc)) <> ";\n\
    \function _print(t) { console.log(_value + (" <> bool <> " ? '\x1b[90m' + t + '\x1b[0m' : t)); }\n\
    \if (_value.length + 3 + _type.length >= 80 || _type.indexOf('\\n') >= 0) {\n\
    \    _print('\\n    : ' + _type.split('\\n').join('\\n      '));\n\
    \} else {\n\
    \    _print(' : ' + _type);\n\
    \}\n"



-- GRAPH TRAVERSAL STATE


data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
    }


emptyState :: State
emptyState =
  State mempty [] Set.empty


stateToBuilder :: State -> B.Builder
stateToBuilder (State revKernels revBuilders _) =
  prependBuilders revKernels (prependBuilders revBuilders mempty)


prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders



-- ADD DEPENDENCIES


type Graph = Map.Map Opt.Global Opt.Node


addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revKernels builders seen) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global $
      State revKernels builders (Set.insert global seen)


addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal mode graph) someState deps
  in
  case graph ! global of
    Opt.Define expr deps ->
      addStmt (addDeps deps state) (
        var global (Expr.generate mode expr)
      )

    Opt.DefineTailFunc argNames body deps ->
      addStmt (addDeps deps state) (
        let (Opt.Global _ name) = global in
        var global (Expr.generateTailDef mode name argNames body)
      )

    Opt.Ctor index arity ->
      addStmt state (
        var global (Expr.generateCtor mode global index arity)
      )

    Opt.Link linkedGlobal ->
      addGlobal mode graph state linkedGlobal

    Opt.Cycle names values functions deps ->
      addStmt (addDeps deps state) (
        generateCycle mode global names values functions
      )

    Opt.Manager effectsType ->
      generateManager mode graph global effectsType state

    Opt.Kernel (Opt.KContent clientChunks clientDeps) maybeServer ->
      if isDebugger global && not (Mode.isDebug mode) then
        state
      else
        case maybeServer of
          Just (Opt.KContent serverChunks serverDeps) | Mode.isServer mode ->
            addKernel (addDeps serverDeps state) (generateKernel mode serverChunks)

          _ ->
            addKernel (addDeps clientDeps state) (generateKernel mode clientChunks)

    Opt.Enum index ->
      addStmt state (
        generateEnum mode global index
      )

    Opt.Box ->
      addStmt state (
        generateBox mode global
      )

    Opt.PortIncoming decoder deps ->
      addStmt (addDeps deps state) (
        generatePort mode global "incomingPort" decoder
      )

    Opt.PortOutgoing encoder deps ->
      addStmt (addDeps deps state) (
        generatePort mode global "outgoingPort" encoder
      )


addStmt :: State -> JS.Stmt -> State
addStmt state stmt =
  addBuilder state (JS.stmtToBuilder stmt)


addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder:revBuilders) seen


addKernel :: State -> B.Builder -> State
addKernel (State revKernels revBuilders seen) kernel =
  State (kernel:revKernels) revBuilders seen


var :: Opt.Global -> Expr.Code -> JS.Stmt
var (Opt.Global home name) code =
  JS.Var [ (Name.fromGlobal home name, Just (Expr.codeToExpr code)) ]


isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  home == N.debugger



-- GENERATE CYCLES


generateCycle :: Mode.Mode -> Opt.Global -> [N.Name] -> [(N.Name, Opt.Expr)] -> [Opt.Def] -> JS.Stmt
generateCycle mode (Opt.Global home _) names values functions =
  JS.Block
    [ JS.Block $ map (generateCycleFunc mode home) functions
    , JS.Block $ map (generateSafeCycle mode home) values
    , case map (generateRealCycle home) values of
        [] ->
          JS.EmptyStmt

        realBlock@(_:_) ->
            case mode of
              Mode.Prod _ _ ->
                JS.Block realBlock

              Mode.Dev _ _ ->
                JS.Try (JS.Block realBlock) Name.dollar $ JS.Throw $ JS.String $
                  "Some top-level definitions from `" <> N.toBuilder (ModuleName._module home) <> "` are causing infinite recursion:\\n"
                  <> drawCycle names
                  <> "\\n\\nThese errors are very tricky, so read "
                  <> B.stringUtf8 (D.makeNakedLink "halting-problem")
                  <> " to learn how to fix it!"
    ]


generateCycleFunc :: Mode.Mode -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
generateCycleFunc mode home def =
  case def of
    Opt.Def name expr ->
      JS.Var [ (Name.fromGlobal home name, Just (Expr.codeToExpr (Expr.generate mode expr))) ]

    Opt.TailDef name args expr ->
      JS.Var [ (Name.fromGlobal home name, Just (Expr.codeToExpr (Expr.generateTailDef mode name args expr))) ]


generateSafeCycle :: Mode.Mode -> ModuleName.Canonical -> (N.Name, Opt.Expr) -> JS.Stmt
generateSafeCycle mode home (name, expr) =
  JS.FunctionStmt (Name.fromCycle home name) [] $
    Expr.codeToStmtList (Expr.generate mode expr)


generateRealCycle :: ModuleName.Canonical -> (N.Name, expr) -> JS.Stmt
generateRealCycle home (name, _) =
  let
    safeName = Name.fromCycle home name
    realName = Name.fromGlobal home name
  in
  JS.Block
    [ JS.Var [ ( realName, Just (JS.Call (JS.Ref safeName) []) ) ]
    , JS.ExprStmt $ JS.Assign (JS.LRef safeName) $
        JS.Function Nothing [] [ JS.Return (Just (JS.Ref realName)) ]
    ]


drawCycle :: [N.Name] -> B.Builder
drawCycle names =
  let
    topLine       = "\\n  ┌─────┐"
    nameLine name = "\\n  │    " <> N.toBuilder name
    midLine       = "\\n  │     ↓"
    bottomLine    = "\\n  └─────┘"
  in
  mconcat (topLine : List.intersperse midLine (map nameLine names) ++ [ bottomLine ])



-- GENERATE KERNEL


generateKernel :: Mode.Mode -> [Opt.KChunk] -> B.Builder
generateKernel mode chunks =
  List.foldl' (addChunk mode) mempty chunks


addChunk :: Mode.Mode -> B.Builder -> Opt.KChunk -> B.Builder
addChunk mode builder chunk =
  case chunk of
    Opt.JS javascript ->
      B.byteString javascript <> builder

    Opt.ElmVar home name ->
      Name.toBuilder (Name.fromGlobal home name) <> builder

    Opt.JsVar home name ->
      Name.toBuilder (Name.fromKernel home name) <> builder

    Opt.ElmField name ->
      Name.toBuilder (Expr.generateField mode name) <> builder

    Opt.JsField int ->
      Name.toBuilder (Name.fromInt int) <> builder

    Opt.JsEnum int ->
      B.intDec int <> builder

    Opt.Debug ->
      case mode of
        Mode.Dev _ _ ->
          builder

        Mode.Prod _ _ ->
          "_UNUSED" <> builder

    Opt.Prod ->
      case mode of
        Mode.Dev _ _ ->
          "_UNUSED" <> builder

        Mode.Prod _ _ ->
          builder



-- GENERATE ENUM


generateEnum :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> JS.Stmt
generateEnum mode global@(Opt.Global home name) index =
  let
    definition =
      case mode of
        Mode.Dev _ _ ->
          Expr.codeToExpr (Expr.generateCtor mode global index 0)

        Mode.Prod _ _ ->
          JS.Int (Index.toMachine index)
  in
  JS.Var [ (Name.fromGlobal home name, Just definition) ]



-- GENERATE BOX


generateBox :: Mode.Mode -> Opt.Global -> JS.Stmt
generateBox mode global@(Opt.Global home name) =
  let
    definition =
      case mode of
        Mode.Dev _ _ ->
          Expr.codeToExpr (Expr.generateCtor mode global Index.first 1)

        Mode.Prod _ _ ->
          JS.Ref (Name.fromGlobal ModuleName.basics N.identity)
  in
  JS.Var [ (Name.fromGlobal home name, Just definition) ]



-- GENERATE PORTS


generatePort :: Mode.Mode -> Opt.Global -> N.Name -> Opt.Expr -> JS.Stmt
generatePort mode (Opt.Global home name) makePort converter =
  let
    definition =
      JS.Call (JS.Ref (Name.fromKernel N.platform makePort))
        [ JS.String (N.toBuilder name)
        , Expr.codeToExpr (Expr.generate mode converter)
        ]
  in
  JS.Var [ (Name.fromGlobal home name, Just definition) ]



-- GENERATE MANAGER


generateManager :: Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager mode graph (Opt.Global home@(ModuleName.Canonical _ moduleName) _) effectsType state =
  let
    managerLVar =
      JS.LBracket
        (JS.Ref (Name.fromKernel N.platform "effectManagers"))
        (JS.String (N.toBuilder moduleName))

    (deps, args, stmts) =
      generateManagerHelp home effectsType

    createManager =
      JS.ExprStmt $ JS.Assign managerLVar $
        JS.Call (JS.Ref (Name.fromKernel N.platform "createManager")) args
  in
  addStmt (List.foldl' (addGlobal mode graph) state deps) $
    JS.Block (createManager : stmts)


generateLeaf :: ModuleName.Canonical -> N.Name -> JS.Stmt
generateLeaf home@(ModuleName.Canonical _ moduleName) name =
  let
    definition =
      JS.Call leaf [ JS.String (N.toBuilder moduleName) ]
  in
  JS.Var [ (Name.fromGlobal home name, Just definition) ]


{-# NOINLINE leaf #-}
leaf :: JS.Expr
leaf =
  JS.Ref (Name.fromKernel N.platform "leaf")


generateManagerHelp :: ModuleName.Canonical -> Opt.EffectsType -> ([Opt.Global], [JS.Expr], [JS.Stmt])
generateManagerHelp home effectsType =
  let
    dep name = Opt.Global home name
    ref name = JS.Ref (Name.fromGlobal home name)
  in
  case effectsType of
    Opt.Cmd ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap" ]
      , [ generateLeaf home "command" ]
      )

    Opt.Sub ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", JS.Int 0, ref "subMap" ]
      , [ generateLeaf home "subscription" ]
      )

    Opt.Fx ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap", ref "subMap" ]
      , [ generateLeaf home "command"
        , generateLeaf home "subscription"
        ]
      )



-- MAIN EXPORTS


toMainExports :: Mode.Mode -> Map.Map ModuleName.Canonical Opt.Main -> B.Builder
toMainExports mode mains =
  let
    export = Name.fromKernel N.platform "export"
    exports = generateExports mode (Map.foldrWithKey addToTrie emptyTrie mains)
  in
  Name.toBuilder export <> "(" <> exports <> ");"


generateExports :: Mode.Mode -> Trie -> B.Builder
generateExports mode (Trie maybeMain subs) =
  let
    starter end =
      case maybeMain of
        Nothing ->
          "{"

        Just (home, main) ->
          "{'init':"
          <> JS.exprToBuilder (Expr.generateMain mode home main)
          <> end
    in
    case Map.toList subs of
      [] ->
        starter "" <> "}"

      (name, subTrie) : otherSubTries ->
        starter "," <>
        "'" <> Text.encodeUtf8Builder name <> "':"
        <> generateExports mode subTrie
        <> List.foldl' (addSubTrie mode) "}" otherSubTries


addSubTrie :: Mode.Mode -> B.Builder -> (Text.Text, Trie) -> B.Builder
addSubTrie mode end (name, trie) =
  ",'" <> Text.encodeUtf8Builder name <> "':" <> generateExports mode trie <> end



-- BUILD TRIES


data Trie =
  Trie
    { _main :: Maybe (ModuleName.Canonical, Opt.Main)
    , _subs :: Map.Map Text.Text Trie
    }


emptyTrie :: Trie
emptyTrie =
  Trie Nothing Map.empty


addToTrie :: ModuleName.Canonical -> Opt.Main -> Trie -> Trie
addToTrie home@(ModuleName.Canonical _ moduleName) main trie =
  merge trie $ segmentsToTrie home (Text.splitOn "." (N.toText moduleName)) main


segmentsToTrie :: ModuleName.Canonical -> [Text.Text] -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    [] ->
      Trie (Just (home, main)) Map.empty

    segment : otherSegments ->
      Trie Nothing (Map.singleton segment (segmentsToTrie home otherSegments main))


merge :: Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie
    (checkedMerge main1 main2)
    (Map.unionWith merge subs1 subs2)


checkedMerge :: Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main) ->
      main

    (main, Nothing) ->
      main

    (Just _, Just _) ->
      error "cannot have two modules with the same name"
