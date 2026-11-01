{-# LANGUAGE ExtendedLiterals, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Generate.JavaScript
  ( generate
  , genForRepl
  , genForReplEndpoint
  )
  where


import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Set as Set

import qualified Crash
import qualified String as S

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified Elm.Interface as I
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as Expr
import qualified Generate.JavaScript.Functions as Functions
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT



-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type Mains = Map.Map ModuleName.Canonical Opt.Main


generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode (Opt.GlobalGraph graph _) mains =
  let
    state = Map.foldrWithKey (addMain mode graph) emptyState mains
  in
  "(function(scope){\n'use strict';"
  <> Functions.functions
  <> perfNote mode
  <> stateToBuilder state
  <> toMainExports mode mains
  <> "}(this));"


addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home N.main)


perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ ->
      ""

    Mode.Dev Nothing ->
      "console.warn('Compiled in DEV mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"

    Mode.Dev (Just _) ->
      "console.warn('Compiled in DEBUG mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"



-- GENERATE FOR REPL


genForRepl :: Bool -> RT.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> N.Name -> I.Annotation -> B.Builder
genForRepl ansi localizer (Opt.GlobalGraph graph _) home name tipe =
  let
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug [N.ascii|toString|])
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  "process.on('uncaughtException', function(err) { process.stderr.write(err.toString() + '\\n'); process.exit(1); });"
  <> Functions.functions
  <> stateToBuilder evalState
  <> print ansi localizer home name tipe


print :: Bool -> RT.Localizer -> ModuleName.Canonical -> N.Name -> I.Annotation -> B.Builder
print ansi localizer home name tipe =
  let
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Module.kernel_debug [N.ascii|toAnsiString|])
    tipeDoc = RT.iToDoc localizer RT.None tipe
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



-- GENERATE FOR REPL ENDPOINT


genForReplEndpoint :: RT.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> Maybe N.Name -> I.Annotation -> B.Builder
genForReplEndpoint localizer (Opt.GlobalGraph graph _) home maybeName tipe =
  let
    name = maybe N.replValueToPrint id maybeName
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug [N.ascii|toString|])
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  Functions.functions
  <> stateToBuilder evalState
  <> postMessage localizer home maybeName tipe


postMessage :: RT.Localizer -> ModuleName.Canonical -> Maybe N.Name -> I.Annotation -> B.Builder
postMessage localizer home maybeName tipe =
  let
    name = maybe N.replValueToPrint id maybeName
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Module.kernel_debug [N.ascii|toAnsiString|])
    tipeDoc = RT.iToDoc localizer RT.None tipe
    toName n = "\"" <> N.toBuilder n <> "\""
  in
  "self.postMessage({\n\
  \  name: " <> maybe "null" toName maybeName <> ",\n\
  \  value: " <> toString <> "(true, " <> value <> "),\n\
  \  type: " <> B.stringUtf8 (show (D.toString tipeDoc)) <> "\n\
  \});\n"



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

    globalToChars (Opt.Global (ModuleName.Canonical p h) n) =
      Module.toChars h ++ "." ++ N.toChars n ++ " in " ++ Pkg.toChars p
  in
  case $(Map.require 'addGlobalHelp) global graph globalToChars of
    Opt.Define expr deps ->
      addStmt (addDeps deps state) $
        var global (Expr.generate mode expr)

    Opt.DefineTailFunc x xs body deps ->
      addStmt (addDeps deps state) $
        let (Opt.Global _ name) = global in
        var global (Expr.genTailDef mode name x xs body)

    Opt.Variant t a -> addStmt state $ var global $ Expr.genVariant mode global t a
    Opt.Enum    t   -> addStmt state $ var global $ Expr.genEnum    mode global t
    Opt.Vector    a -> addStmt state $ var global $ Expr.genVector  mode global   a
    Opt.Void        -> addStmt state $ var global $ Expr.genVoid    mode global

    Opt.Box ->
      addStmt (addGlobal mode graph state identity) $
        genBox mode global

    Opt.Link linkedGlobal ->
      addGlobal mode graph state linkedGlobal

    Opt.Cycle n ns values functions deps ->
      addStmt (addDeps deps state) $
        genCycle mode global n ns values functions

    Opt.Manager effectsType ->
      genManager mode graph global effectsType state

    Opt.Kernel chunks deps ->
      if isDebugger global && not (Mode.isDebug mode)
      then state
      else addKernel (addDeps deps state) (genKernel mode chunks)

    Opt.PortIncoming decoder deps ->
      addStmt (addDeps deps state) $
        genPort mode global [N.ascii|incomingPort|] decoder

    Opt.PortOutgoing encoder deps ->
      addStmt (addDeps deps state) $
        genPort mode global [N.ascii|outgoingPort|] encoder


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
  JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  Module.toString home == Module.kernelToString Module.kernel_debugger



-- GENERATE CYCLES


genCycle :: Mode.Mode -> Opt.Global -> N.Name -> [N.Name] -> [(N.Name, Opt.Expr)] -> [Opt.Def] -> JS.Stmt
genCycle mode (Opt.Global home _) n ns values functions =
  JS.Block
    [ JS.Block $ map (genCycleFunc mode home) functions
    , JS.Block $ map (genSafeCycle mode home) values
    , case map (genRealCycle home) values of
        [] ->
          JS.EmptyStmt

        realBlock@(_:_) ->
            case mode of
              Mode.Prod _ ->
                JS.Block realBlock

              Mode.Dev _ ->
                JS.Try (JS.Block realBlock) JsName.dollar $ JS.Throw $ JS.String $
                  "Some top-level definitions from `" <> Module.toBuilder (ModuleName._module home) <> "` are causing infinite recursion:\\n"
                  <> drawCycle n ns
                  <> "\\n\\nThese errors are very tricky, so read "
                  <> B.stringUtf8 (D.makeNakedLink "bad-recursion")
                  <> " to learn how to fix it!"
    ]


genCycleFunc :: Mode.Mode -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
genCycleFunc mode home def =
  case def of
    Opt.Def name expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generate mode expr))

    Opt.TailDef name x xs expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.genTailDef mode name x xs expr))


genSafeCycle :: Mode.Mode -> ModuleName.Canonical -> (N.Name, Opt.Expr) -> JS.Stmt
genSafeCycle mode home (name, expr) =
  JS.FunctionStmt (JsName.fromCycle home name) [] $
    Expr.codeToStmtList (Expr.generate mode expr)


genRealCycle :: ModuleName.Canonical -> (N.Name, expr) -> JS.Stmt
genRealCycle home (name, _) =
  let
    safeName = JsName.fromCycle home name
    realName = JsName.fromGlobal home name
  in
  JS.Block
    [ JS.Var realName (JS.Call (JS.Ref safeName) [])
    , JS.ExprStmt $ JS.Assign (JS.LRef safeName) $
        JS.Function Nothing [] [ JS.Return (JS.Ref realName) ]
    ]


drawCycle :: N.Name -> [N.Name] -> B.Builder
drawCycle n ns =
  let
    topLine       = "\\n  ┌─────┐"
    nameLine name = "\\n  │    " <> N.toBuilder name
    midLine       = "\\n  │     ↓"
    bottomLine    = "\\n  └─────┘"
  in
  mconcat (topLine : List.intersperse midLine (map nameLine (n:ns)) ++ [ bottomLine ])



-- GENERATE KERNEL


genKernel :: Mode.Mode -> [K.Chunk] -> B.Builder
genKernel mode chunks =
  List.foldr (addChunk mode) mempty chunks


addChunk :: Mode.Mode -> K.Chunk -> B.Builder -> B.Builder
addChunk mode chunk builder =
  case chunk of
    K.JS javascript ->
      B.byteString javascript <> builder

    K.ElmVar home name ->
      JsName.toBuilder (JsName.fromGlobal home name) <> builder

    K.JsVar home name ->
      JsName.toBuilder (JsName.fromKernel home name) <> builder

    K.ElmField name ->
      JsName.toBuilder (Expr.genField mode name) <> builder

    K.JsField int ->
      JsName.toBuilder (JsName.fromInt int) <> builder

    K.JsEnum int ->
      B.intDec int <> builder

    K.Debug ->
      case mode of
        Mode.Dev _ ->
          builder

        Mode.Prod _ ->
          "_UNUSED" <> builder

    K.Prod ->
      case mode of
        Mode.Dev _ ->
          "_UNUSED" <> builder

        Mode.Prod _ ->
          builder



-- GENERATE BOX


genBox :: Mode.Mode -> Opt.Global -> JS.Stmt
genBox mode global@(Opt.Global home name) =
  JS.Var (JsName.fromGlobal home name) $
    case mode of
      Mode.Dev  _ -> Expr.codeToExpr (Expr.genVariant mode global Tag.zero 1)
      Mode.Prod _ -> JS.Ref (JsName.fromGlobal ModuleName.basics N.identity)


{-# NOINLINE identity #-}
identity :: Opt.Global
identity =
  Opt.Global ModuleName.basics N.identity



-- GENERATE PORTS


genPort :: Mode.Mode -> Opt.Global -> N.Name -> Opt.Expr -> JS.Stmt
genPort mode (Opt.Global home name) makePort converter =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call (JS.Ref (JsName.fromKernel Module.kernel_platform makePort))
      [ JS.String (N.toBuilder name)
      , Expr.codeToExpr (Expr.generate mode converter)
      ]



-- GENERATE MANAGER


genManager :: Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
genManager mode graph (Opt.Global home@(ModuleName.Canonical _ moduleName) _) effectsType state =
  let
    managerLVar =
      JS.LBracket
        (JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|effectManagers|]))
        (JS.String (Module.toBuilder moduleName))

    (deps, args, stmts) =
      genManagerHelp home effectsType

    createManager =
      JS.ExprStmt $ JS.Assign managerLVar $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|createManager|])) args
  in
  addStmt (List.foldl' (addGlobal mode graph) state deps) $
    JS.Block (createManager : stmts)


genLeaf :: ModuleName.Canonical -> N.Name -> JS.Stmt
genLeaf home@(ModuleName.Canonical _ moduleName) name =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call leaf [ JS.String (Module.toBuilder moduleName) ]



{-# NOINLINE leaf #-}
leaf :: JS.Expr
leaf =
  JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|leaf|])


genManagerHelp :: ModuleName.Canonical -> Opt.EffectsType -> ([Opt.Global], [JS.Expr], [JS.Stmt])
genManagerHelp home effectsType =
  let
    dep name = Opt.Global home name
    ref name = JS.Ref (JsName.fromGlobal home name)
  in
  case effectsType of
    Opt.Cmd ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|cmdMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], ref [N.ascii|cmdMap|] ]
      , [ genLeaf home [N.ascii|command|] ]
      )

    Opt.Sub ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|subMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], JS.Int 0, ref [N.ascii|subMap|] ]
      , [ genLeaf home [N.ascii|subscription|] ]
      )

    Opt.Fx ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|cmdMap|], dep [N.ascii|subMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], ref [N.ascii|cmdMap|], ref [N.ascii|subMap|] ]
      , [ genLeaf home [N.ascii|command|]
        , genLeaf home [N.ascii|subscription|]
        ]
      )



-- MAIN EXPORTS


toMainExports :: Mode.Mode -> Mains -> B.Builder
toMainExports mode mains =
  let
    export = JsName.fromKernel Module.kernel_platform [N.ascii|export|]
    exports = genExports mode (Map.foldrWithKey addToTrie emptyTrie mains)
  in
  JsName.toBuilder export <> "(" <> exports <> ");"


genExports :: Mode.Mode -> Trie -> B.Builder
genExports mode (Trie maybeMain subs) =
  let
    starter end =
      case maybeMain of
        Nothing ->
          "{"

        Just (home, main) ->
          "{'init':"
          <> JS.exprToBuilder (Expr.genMain mode home main)
          <> end
    in
    case Map.toList subs of
      [] ->
        starter "" <> "}"

      (name, subTrie) : otherSubTries ->
        starter "," <>
        "'" <> S.toBuilder name <> "':"
        <> genExports mode subTrie
        <> List.foldl' (addSubTrie mode) "}" otherSubTries


addSubTrie :: Mode.Mode -> B.Builder -> (S.String, Trie) -> B.Builder
addSubTrie mode end (name, trie) =
  ",'" <> S.toBuilder name <> "':" <> genExports mode trie <> end



-- BUILD TRIES


data Trie =
  Trie
    { _main :: Maybe (ModuleName.Canonical, Opt.Main)
    , _subs :: Map.Map S.String Trie
    }


emptyTrie :: Trie
emptyTrie =
  Trie Nothing Map.empty


addToTrie :: ModuleName.Canonical -> Opt.Main -> Trie -> Trie
addToTrie home@(ModuleName.Canonical _ h) main trie =
  merge trie $ segmentsToTrie home (S.split 0x2E#Word8 {-.-} (Module.toString h)) main


segmentsToTrie :: ModuleName.Canonical -> [S.String] -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    []       -> Trie (Just (home, main)) Map.empty
    seg:segs -> Trie Nothing (Map.singleton seg (segmentsToTrie home segs main))


merge :: Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie
    (checkedMerge main1 main2)
    (Map.unionWith merge subs1 subs2)


checkedMerge :: Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main)  -> main
    (main, Nothing)  -> main
    (Just _, Just _) -> $(Crash.crash 'checkedMerge) "cannot have two modules with the same name"

