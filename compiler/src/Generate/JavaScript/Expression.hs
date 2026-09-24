{-# OPTIONS_GHC -fno-warn-x-partial #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Generate.JavaScript.Expression
  ( generate
  , generateCtor
  , generateField
  , generateTailDef
  , generateMain
  , Code
  , codeToExpr
  , codeToStmtList
  )
  where


import Prelude hiding (and, or, not, negate, truncate)
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Char as Char
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Version as V
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import Json.Encode ((==>))
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A



-- EXPRESSIONS


generateJsExpr :: Mode.Mode -> Opt.Expr -> JS.Expr
generateJsExpr mode expression =
  codeToExpr (generate mode expression)


generate :: Mode.Mode -> Opt.Expr -> Code
generate mode expression =
  case expression of
    Opt.Bool b -> JsExpr $ JS.Bool b
    Opt.Chr c ->
      JsExpr $
        case mode of
          Mode.Dev  _ -> JS.Call toChar [ JS.String (P.primBounded charUtf8 c) ]
          Mode.Prod _ -> JS.String (P.primBounded charUtf8 c)

    Opt.Str      s -> JsExpr $ JS.String (Utf8.toBuilder s)
    Opt.Int      i -> JsExpr $ JS.Int i
    Opt.Float    f -> JsExpr $ JS.Float (Utf8.toBuilder f)
    Opt.VarLocal x -> JsExpr $ JS.Ref (JsName.fromLocal x)

    Opt.VarGlobal (Opt.Global h n) ->
      JsExpr $ JS.Ref (JsName.fromGlobal h n)

    Opt.VarEnum (Opt.Global home name) index ->
      case mode of
        Mode.Dev  _ -> JsExpr $ JS.Ref (JsName.fromGlobal home name)
        Mode.Prod _ -> JsExpr $ JS.Int (Index.toMachine index)

    Opt.VarBox (Opt.Global home name) ->
      JsExpr $ JS.Ref $
        case mode of
          Mode.Dev  _ -> JsName.fromGlobal home name
          Mode.Prod _ -> JsName.fromGlobal ModuleName.basics N.identity

    Opt.VarCycle h n     -> JsExpr $ JS.Call (JS.Ref (JsName.fromCycle h n)) []
    Opt.VarDebug n h r u -> JsExpr $ generateDebug n h r u
    Opt.VarKernel  h n   -> JsExpr $ JS.Ref (JsName.fromKernel h n)

    Opt.List entries ->
      case entries of
        [] ->
          JsExpr $ JS.Ref (JsName.fromKernel Module.kernel_list [N.ascii|Nil|])

        _ ->
          JsExpr $
            JS.Call
              (JS.Ref (JsName.fromKernel Module.kernel_list N.fromArray))
              [ JS.Array $ map (generateJsExpr mode) entries
              ]

    Opt.Function args body ->
      generateFunction (map JsName.fromLocal args) (generate mode body)

    Opt.Call f xs     -> JsExpr $ generateCall mode f xs
    Opt.TailCall n xs -> JsBlock $ generateTailCall mode n xs
    Opt.If bs f       -> generateIf mode bs f

    Opt.Let def body ->
      JsBlock $
        generateDef mode def : codeToStmtList (generate mode body)

    Opt.Destruct (Opt.Destructor name path) body ->
      let
        pathDef = JS.Var (JsName.fromLocal name) (generatePath mode path)
      in
      JsBlock $ pathDef : codeToStmtList (generate mode body)

    Opt.Case label root decider jumps ->
      JsBlock $ generateCase mode label root decider jumps

    Opt.Accessor field ->
      JsExpr $ JS.Function Nothing [JsName.dollar]
        [ JS.Return $
            JS.Access (JS.Ref JsName.dollar) (generateField mode field)
        ]

    Opt.Access record field ->
      JsExpr $ JS.Access (generateJsExpr mode record) (generateField mode field)

    Opt.Update record fields ->
      JsExpr $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils N.update))
          [ generateJsExpr mode record
          , generateRecord mode fields
          ]

    Opt.Record fields ->
      JsExpr $ generateRecord mode fields

    Opt.Unit ->
      case mode of
        Mode.Dev  _ -> JsExpr $ JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple0|])
        Mode.Prod _ -> JsExpr $ JS.Int 0

    Opt.Tuple a b maybeC ->
      JsExpr $
        case maybeC of
          Nothing ->
            JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple2|]))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              ]

          Just c ->
            JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple3|]))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              , generateJsExpr mode c
              ]

    Opt.Shader src attributes uniforms ->
      let
        toTranlation field =
          ( JsName.fromLocal field
          , JS.String (JsName.toBuilder (generateField mode field))
          )

        toTranslationObject fields =
          JS.Object (map toTranlation (Set.toList fields))
      in
      JsExpr $ JS.Object $
        [ ( JsName.fromLocal [N.ascii|src|], JS.String (Shader.toJsStringBuilder src) )
        , ( JsName.fromLocal [N.ascii|attributes|], toTranslationObject attributes )
        , ( JsName.fromLocal [N.ascii|uniforms|], toTranslationObject uniforms )
        ]



-- CODE CHUNKS


data Code
  = JsExpr JS.Expr
  | JsBlock [JS.Stmt]


codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr             expr  -> expr
    JsBlock [JS.Return expr] -> expr
    JsBlock stmts            -> JS.Call (JS.Function Nothing [] stmts) []


codeToStmtList :: Code -> [JS.Stmt]
codeToStmtList code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
        stmts

    JsExpr expr ->
        [ JS.Return expr ]

    JsBlock stmts ->
        stmts


codeToStmt :: Code -> JS.Stmt
codeToStmt code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
        JS.Block stmts

    JsExpr expr ->
        JS.Return expr

    JsBlock [stmt] ->
        stmt

    JsBlock stmts ->
        JS.Block stmts



-- CHARS


{-# NOINLINE toChar #-}
toChar :: JS.Expr
toChar =
  JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|chr|])



-- CTOR


generateCtor :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

    ctorTag =
      case mode of
        Mode.Dev  _ -> JS.String (N.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)
  in
  generateFunction argNames $ JsExpr $ JS.Object $
    (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames


ctorToInt :: ModuleName.Canonical -> N.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
  if home == ModuleName.dict && (name == rbNode || name == rbEmpty) then
    0 - Index.toHuman index
  else
    Index.toMachine index


{-# NOINLINE rbNode  #-}; rbNode  :: N.Name; rbNode  = [N.ascii|RBNode_elm_builtin|]
{-# NOINLINE rbEmpty #-}; rbEmpty :: N.Name; rbEmpty = [N.ascii|RBEmpty_elm_builtin|]



-- RECORDS


generateRecord :: Mode.Mode -> Map.Map N.Name Opt.Expr -> JS.Expr
generateRecord mode fields =
  let
    toPair (field, value) =
      (generateField mode field, generateJsExpr mode value)
  in
  JS.Object (map toPair (Map.toList fields))


generateField :: Mode.Mode -> N.Name -> JsName.Name
generateField mode name =
  case mode of
    Mode.Dev _       -> JsName.fromLocal name
    Mode.Prod fields -> $(Map.require 'generateField) name fields N.toChars




-- DEBUG


generateDebug :: N.Name -> ModuleName.Canonical -> A.Region -> Maybe N.Name -> JS.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= N.todo then
    JS.Ref (JsName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_debug N.todo)) $
          [ JS.String (Module.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_debug [N.ascii|todoCase|])) $
          [ JS.String (Module.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (JsName.fromLocal valueName)
          ]


regionToJsExpr :: A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ [N.ascii|start|] ===> JS.Object [ [N.ascii|line|] ===> JS.Int sr, [N.ascii|column|] ===> JS.Int sc ]
    , [N.ascii|end|]   ===> JS.Object [ [N.ascii|line|] ===> JS.Int er, [N.ascii|column|] ===> JS.Int ec ]
    ]
  where
    (===>) n v = (JsName.fromLocal n, v)
    (sr, sc) = A.toEditorRowCol start
    (er, ec) = A.toEditorRowCol end



-- FUNCTION


generateFunction :: [JsName.Name] -> Code -> Code
generateFunction args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call helper
          [ JS.Function Nothing args $
              codeToStmtList body
          ]

    Nothing ->
      let
        addArg arg code =
          JsExpr $ JS.Function Nothing [arg] $
            codeToStmtList code
      in
      foldr addArg body args


{-# NOINLINE funcHelpers #-}
funcHelpers :: IntMap.IntMap JS.Expr
funcHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeF n))) [2..9]



-- CALLS


generateCall :: Mode.Mode -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCall mode func args =
  case func of
    Opt.VarGlobal global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      generateCoreCall mode global args

    Opt.VarBox _ ->
      case mode of
        Mode.Dev  _ -> generateCallHelp mode func args
        Mode.Prod _ ->
          case args of
            [arg] -> generateJsExpr mode arg
            _     -> generateCallHelp mode func args

    _ ->
      generateCallHelp mode func args


generateCallHelp :: Mode.Mode -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCallHelp mode func args =
  generateNormalCall
    (generateJsExpr mode func)
    (map (generateJsExpr mode) args)


generateGlobalCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateGlobalCall home name args =
  generateNormalCall (JS.Ref (JsName.fromGlobal home name)) args


generateNormalCall :: JS.Expr -> [JS.Expr] -> JS.Expr
generateNormalCall func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper -> JS.Call helper (func:args)
    Nothing     -> List.foldl' (\f a -> JS.Call f [a]) func args


{-# NOINLINE callHelpers #-}
callHelpers :: IntMap.IntMap JS.Expr
callHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeA n))) [2..9]



-- CORE CALLS


generateCoreCall :: Mode.Mode -> Opt.Global -> [Opt.Expr] -> JS.Expr
generateCoreCall mode (Opt.Global home@(ModuleName.Canonical _ h) name) args
  | h == Module.basics  = generateBasicsCall mode home name args
  | h == Module.bitwise = generateBitwiseCall home name (map (generateJsExpr mode) args)
  | h == Module.tuple   = generateTupleCall   home name (map (generateJsExpr mode) args)
  | h == Module.jsArray = generateJsArrayCall home name (map (generateJsExpr mode) args)
  | otherwise           = generateGlobalCall  home name (map (generateJsExpr mode) args)


generateTupleCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateTupleCall home name args =
  case args of
    [value]
      | name == N.first  -> JS.Access value (JsName.fromLocal N.a)
      | name == N.second -> JS.Access value (JsName.fromLocal N.b)
      | otherwise        -> generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateJsArrayCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateJsArrayCall home name args =
  case args of
    [entry]        | name == [N.ascii|singleton|] -> JS.Array [entry]
    [index, array] | name == [N.ascii|unsafeGet|] -> JS.Index array index
    _                                             -> generateGlobalCall home name args


generateBitwiseCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateBitwiseCall h n args =
  case args of
    [arg]
      | n == complement -> JS.Prefix JS.PrefixComplement arg
      | otherwise       -> generateGlobalCall h n args

    [left,right]
      | n == and            -> JS.Infix JS.OpBitwiseAnd left right
      | n == or             -> JS.Infix JS.OpBitwiseOr  left right
      | n == xor            -> JS.Infix JS.OpBitwiseXor left right
      | n == shiftLeftBy    -> JS.Infix JS.OpLShift     right left
      | n == shiftRightBy   -> JS.Infix JS.OpSpRShift   right left
      | n == shiftRightZfBy -> JS.Infix JS.OpZfRShift   right left
      | otherwise           -> generateGlobalCall h n args

    _ ->
      generateGlobalCall h n args


{-# NOINLINE complement     #-}; complement     :: N.Name; complement     = [N.ascii|complement|]
{-# NOINLINE shiftLeftBy    #-}; shiftLeftBy    :: N.Name; shiftLeftBy    = [N.ascii|shiftLeftBy|]
{-# NOINLINE shiftRightBy   #-}; shiftRightBy   :: N.Name; shiftRightBy   = [N.ascii|shiftRightBy|]
{-# NOINLINE shiftRightZfBy #-}; shiftRightZfBy :: N.Name; shiftRightZfBy = [N.ascii|shiftRightZfBy|]


generateBasicsCall :: Mode.Mode -> ModuleName.Canonical -> N.Name -> [Opt.Expr] -> JS.Expr
generateBasicsCall mode h n args =
  case args of
    [elmArg] ->
      case generateJsExpr mode elmArg of
        arg
          | n == not      -> JS.Prefix JS.PrefixNot arg
          | n == negate   -> JS.Prefix JS.PrefixNegate arg
          | n == toFloat  -> arg
          | n == truncate -> JS.Infix JS.OpBitwiseOr arg (JS.Int 0)
          | otherwise     -> generateGlobalCall h n [arg]

    [elmLeft, elmRight]
      -- NOTE: removed "composeL" and "composeR" because of this issue:
      -- https://github.com/elm/compiler/issues/1722
      | n == append -> genAppend mode elmLeft elmRight
      | n == apL    -> generateJsExpr mode $ apply elmLeft elmRight
      | n == apR    -> generateJsExpr mode $ apply elmRight elmLeft
      | otherwise   ->
          let
            left  = generateJsExpr mode elmLeft
            right = generateJsExpr mode elmRight
          in
          case () of
            ()
              | n == add         -> JS.Infix JS.OpAdd left right
              | n == sub         -> JS.Infix JS.OpSub left right
              | n == mul         -> JS.Infix JS.OpMul left right
              | n == fdiv        -> JS.Infix JS.OpDiv left right
              | n == idiv        -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
              | n == eq          -> equal left right
              | n == neq         -> notEqual left right
              | n == lt          -> comp JS.OpLt JS.OpLt   0  left right
              | n == gt          -> comp JS.OpGt JS.OpGt   0  left right
              | n == le          -> comp JS.OpLe JS.OpLt   1  left right
              | n == ge          -> comp JS.OpGe JS.OpGt (-1) left right
              | n == or          -> JS.Infix JS.OpOr  left right
              | n == and         -> JS.Infix JS.OpAnd left right
              | n == xor         -> JS.Infix JS.OpNe  left right
              | n == remainderBy -> JS.Infix JS.OpMod right left
              | otherwise        -> generateGlobalCall h n [left, right]

    _ ->
      generateGlobalCall h n (map (generateJsExpr mode) args)


{-# NOINLINE not         #-}; not         :: N.Name; not         = [N.ascii|not|]
{-# NOINLINE negate      #-}; negate      :: N.Name; negate      = [N.ascii|negate|]
{-# NOINLINE toFloat     #-}; toFloat     :: N.Name; toFloat     = [N.ascii|toFloat|]
{-# NOINLINE truncate    #-}; truncate    :: N.Name; truncate    = [N.ascii|truncate|]
{-# NOINLINE append      #-}; append      :: N.Name; append      = [N.ascii|append|]
{-# NOINLINE apL         #-}; apL         :: N.Name; apL         = [N.ascii|apL|]
{-# NOINLINE apR         #-}; apR         :: N.Name; apR         = [N.ascii|apR|]
{-# NOINLINE add         #-}; add         :: N.Name; add         = [N.ascii|add|]
{-# NOINLINE sub         #-}; sub         :: N.Name; sub         = [N.ascii|sub|]
{-# NOINLINE mul         #-}; mul         :: N.Name; mul         = [N.ascii|mul|]
{-# NOINLINE fdiv        #-}; fdiv        :: N.Name; fdiv        = [N.ascii|fdiv|]
{-# NOINLINE idiv        #-}; idiv        :: N.Name; idiv        = [N.ascii|idiv|]
{-# NOINLINE eq          #-}; eq          :: N.Name; eq          = [N.ascii|eq|]
{-# NOINLINE neq         #-}; neq         :: N.Name; neq         = [N.ascii|neq|]
{-# NOINLINE lt          #-}; lt          :: N.Name; lt          = [N.ascii|lt|]
{-# NOINLINE gt          #-}; gt          :: N.Name; gt          = [N.ascii|gt|]
{-# NOINLINE le          #-}; le          :: N.Name; le          = [N.ascii|le|]
{-# NOINLINE ge          #-}; ge          :: N.Name; ge          = [N.ascii|ge|]
{-# NOINLINE or          #-}; or          :: N.Name; or          = [N.ascii|or|]
{-# NOINLINE and         #-}; and         :: N.Name; and         = [N.ascii|and|]
{-# NOINLINE xor         #-}; xor         :: N.Name; xor         = [N.ascii|xor|]
{-# NOINLINE remainderBy #-}; remainderBy :: N.Name; remainderBy = [N.ascii|remainderBy|]

{-# NOINLINE cmp      #-}; cmp      :: N.Name; cmp      = [N.ascii|cmp|]
{-# NOINLINE ap       #-}; ap       :: N.Name; ap       = [N.ascii|ap|]
{-# NOINLINE valueOf  #-}; valueOf  :: N.Name; valueOf  = [N.ascii|valueOf|]


equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils eq)) [left, right]


notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixNot $
      JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils eq)) [left, right]


comp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
comp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils cmp)) [left, right])
      (JS.Int backupInt)




isLiteral :: JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.String _ -> True
    JS.Float  _ -> True
    JS.Int    _ -> True
    JS.Bool   _ -> True
    _           -> False


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field -> Opt.Access value field
    Opt.Call f args    -> Opt.Call f (args ++ [value])
    _                  -> Opt.Call func [value]


genAppend :: Mode.Mode -> Opt.Expr -> Opt.Expr -> JS.Expr
genAppend mode left right =
  if any isStringLiteral seqs
    then foldr1 (JS.Infix JS.OpAdd) seqs
    else foldr1 utils_ap seqs
  where
    seqs =
      generateJsExpr mode left : toSeqs mode right

    utils_ap a b =
      JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils ap)) [a, b]


toSeqs :: Mode.Mode -> Opt.Expr -> [JS.Expr]
toSeqs mode expr =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global h n)) [left, right]
      | h == ModuleName.basics && n == append ->
          generateJsExpr mode left : toSeqs mode right

    _ ->
      [generateJsExpr mode expr]


isStringLiteral :: JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.String _ -> True
    _           -> False



-- SIMPLIFY INFIX OPERATORS


strictEq :: JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.Int  0 -> JS.Prefix JS.PrefixNot right
    JS.Bool b -> if b then right else JS.Prefix JS.PrefixNot right
    _ ->
      case right of
        JS.Int  0 -> JS.Prefix JS.PrefixNot left
        JS.Bool b -> if b then left else JS.Prefix JS.PrefixNot left
        _         -> JS.Infix JS.OpEq left right


strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int  0 -> JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)
    JS.Bool b -> if b then JS.Prefix JS.PrefixNot right else right
    _ ->
      case right of
        JS.Int  0 -> JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)
        JS.Bool b -> if b then JS.Prefix JS.PrefixNot left else left
        _         -> JS.Infix JS.OpNe left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
generateTailCall :: Mode.Mode -> N.Name -> [(N.Name, Opt.Expr)] -> [JS.Stmt]
generateTailCall mode name args =
  let
    toTempVars (argName, arg) =
      ( JsName.makeTemp argName, generateJsExpr mode arg )

    toRealVars (argName, _) =
      JS.ExprStmt $
        JS.Assign (JS.LRef (JsName.fromLocal argName)) (JS.Ref (JsName.makeTemp argName))
  in
  JS.Vars (map toTempVars args)
  : map toRealVars args
  ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef :: Mode.Mode -> Opt.Def -> JS.Stmt
generateDef mode def =
  case def of
    Opt.Def name body ->
      JS.Var (JsName.fromLocal name) (generateJsExpr mode body)

    Opt.TailDef name argNames body ->
      JS.Var (JsName.fromLocal name) (codeToExpr (generateTailDef mode name argNames body))


generateTailDef :: Mode.Mode -> N.Name -> [N.Name] -> Opt.Expr -> Code
generateTailDef mode name argNames body =
  generateFunction (map JsName.fromLocal argNames) $ JsBlock $
    [ JS.Labelled (JsName.fromLocal name) $
        JS.While (JS.Bool True) $
          codeToStmt $ generate mode body
    ]



-- PATHS


generatePath :: Mode.Mode -> Opt.Path -> JS.Expr
generatePath mode path =
  case path of
    Opt.Root  n   -> JS.Ref (JsName.fromLocal n)
    Opt.Index i p -> JS.Access (generatePath mode p) (JsName.fromIndex i)
    Opt.Field f p -> JS.Access (generatePath mode p) (generateField mode f)
    Opt.Unbox p ->
      case mode of
        Mode.Dev  _ -> JS.Access (generatePath mode p) (JsName.fromIndex Index.first)
        Mode.Prod _ -> generatePath mode p



-- GENERATE IFS


generateIf :: Mode.Mode -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
generateIf mode givenBranches givenFinal =
  let
    (branches, final) =
      crushIfs givenBranches givenFinal

    convertBranch (condition, expr) =
      ( generateJsExpr mode condition
      , generate mode expr
      )

    branchExprs = map convertBranch branches
    finalCode = generate mode final
  in
  if isBlock finalCode || any (isBlock . snd) branchExprs then
    JsBlock [ foldr addStmtIf (codeToStmt finalCode) branchExprs ]
  else
    JsExpr $ foldr addExprIf (codeToExpr finalCode) branchExprs


addExprIf :: (JS.Expr, Code) -> JS.Expr -> JS.Expr
addExprIf (condition, branch) final =
  JS.If condition (codeToExpr branch) final


addStmtIf :: (JS.Expr, Code) -> JS.Stmt -> JS.Stmt
addStmtIf (condition, branch) final =
  JS.IfStmt condition (codeToStmt branch) final


isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr  _ -> False


crushIfs :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final


crushIfsHelp
    :: [(Opt.Expr, Opt.Expr)]
    -> [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
        case final of
          Opt.If subBranches subFinal ->
              crushIfsHelp visitedBranches subBranches subFinal

          _ ->
              (reverse visitedBranches, final)

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited final



-- CASE EXPRESSIONS


generateCase :: Mode.Mode -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
generateCase mode label root decider jumps =
  foldr (goto mode label) (generateDecider mode label root decider) jumps


goto :: Mode.Mode -> N.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (JsName.makeLabel label index)
        (JS.While (JS.Bool True) (JS.Block stmts))
  in
  labeledDeciderStmt : codeToStmtList (generate mode branch)


generateDecider :: Mode.Mode -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
generateDecider mode label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode branch)

    Opt.Leaf (Opt.Jump index) ->
      [ JS.Break (Just (JsName.makeLabel label index)) ]

    Opt.Chain testChain success failure ->
      [ JS.IfStmt
          (List.foldl1' (JS.Infix JS.OpAnd) (map (generateIfTest mode root) testChain))
          (JS.Block $ generateDecider mode label root success)
          (JS.Block $ generateDecider mode label root failure)
      ]

    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (generateCaseTest mode root path (fst (head edges)))
          ( foldr
              (\edge cases -> generateCaseBranch mode label root edge : cases)
              [ JS.Default (generateDecider mode label root fallback) ]
              edges
          )
      ]


generateIfTest :: Mode.Mode -> N.Name -> (DT.Path, DT.Test) -> JS.Expr
generateIfTest mode root (path, test) =
  let
    value = pathToJsExpr mode root path
  in
  case test of
    DT.IsCtor home name index _ opts ->
      let
        tag =
          case mode of
            Mode.Dev  _ -> JS.Access value JsName.dollar
            Mode.Prod _ ->
              case opts of
                Can.Normal -> JS.Access value JsName.dollar
                Can.Enum   -> value
                Can.Unbox  -> value
      in
      strictEq tag $
        case mode of
          Mode.Dev _ -> JS.String (N.toBuilder name)
          Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsBool True  -> value
    DT.IsBool False -> JS.Prefix JS.PrefixNot value
    DT.IsInt i      -> strictEq value (JS.Int i)

    DT.IsChr char ->
      strictEq (JS.String (P.primBounded charUtf8 char)) $
        case mode of
          Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal valueOf)) []
          Mode.Prod _ -> value

    DT.IsStr string ->
      strictEq value (JS.String (Utf8.toBuilder string))

    DT.IsCons ->
      JS.Access value (JsName.fromLocal N.b)

    DT.IsNil ->
      JS.Prefix JS.PrefixNot $
        JS.Access value (JsName.fromLocal N.b)

    DT.IsTuple ->
      $(Crash.crash 'generateIfTest) "COMPILER BUG - there should never be tests on a tuple"



generateCaseBranch :: Mode.Mode -> N.Name -> N.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
generateCaseBranch mode label root (test, subTree) =
  JS.Case
    (generateCaseValue mode test)
    (generateDecider mode label root subTree)


generateCaseValue :: Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
  case test of
    DT.IsCtor home name index _ _ ->
      case mode of
        Mode.Dev  _ -> JS.String (N.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsInt  i -> JS.Int i
    DT.IsChr  c -> JS.String (P.primBounded charUtf8 c)
    DT.IsStr  s -> JS.String (Utf8.toBuilder s)
    DT.IsBool _ -> $(Crash.crash 'generateCaseValue) "COMPILER BUG - there should never be three tests on a boolean"
    DT.IsCons   -> $(Crash.crash 'generateCaseValue) "COMPILER BUG - there should never be three tests on a list"
    DT.IsNil    -> $(Crash.crash 'generateCaseValue) "COMPILER BUG - there should never be three tests on a list"
    DT.IsTuple  -> $(Crash.crash 'generateCaseValue) "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest :: Mode.Mode -> N.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
  let
    value = pathToJsExpr mode root path
  in
  case exampleTest of
    DT.IsCtor _ _ _ _ opts ->
      case mode of
        Mode.Dev  _ -> JS.Access value JsName.dollar
        Mode.Prod _ ->
          case opts of
            Can.Normal -> JS.Access value JsName.dollar
            Can.Enum   -> value
            Can.Unbox  -> value

    DT.IsInt _ -> value
    DT.IsStr _ -> value
    DT.IsChr _ ->
      case mode of
        Mode.Dev  _ -> JS.Call (JS.Access value (JsName.fromLocal valueOf)) []
        Mode.Prod _ -> value

    DT.IsBool _ -> $(Crash.crash 'generateCaseTest) "COMPILER BUG - there should never be three tests on a list"
    DT.IsCons   -> $(Crash.crash 'generateCaseTest) "COMPILER BUG - there should never be three tests on a list"
    DT.IsNil    -> $(Crash.crash 'generateCaseTest) "COMPILER BUG - there should never be three tests on a list"
    DT.IsTuple  -> $(Crash.crash 'generateCaseTest) "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr :: Mode.Mode -> N.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index i p ->
      JS.Access (pathToJsExpr mode root p) (JsName.fromIndex i)

    DT.Unbox p ->
      case mode of
        Mode.Dev  _ -> JS.Access (pathToJsExpr mode root p) (JsName.fromIndex Index.first)
        Mode.Prod _ -> pathToJsExpr mode root p

    DT.Empty ->
      JS.Ref (JsName.fromLocal root)



-- GENERATE CHAR


charUtf8 :: P.BoundedPrim Char
charUtf8 =
    P.condB (== '\\') (esc 0x5C) $
    P.condB (== '\'') (esc 0x27) $
    P.condB (>= ' ' ) P.charUtf8 $
    P.condB (== '\b') (esc 0x62) $
    P.condB (== '\f') (esc 0x66) $
    P.condB (== '\n') (esc 0x6E) $
    P.condB (== '\r') (esc 0x72) $
    P.condB (== '\t') (esc 0x74) $ fallback
  where
    {-# INLINE esc #-}
    esc w =
      P.liftFixedToBounded $ const (0x5C,w) >$< P.word8 >*< P.word8

    {-# INLINE fallback #-}
    fallback =
      P.liftFixedToBounded $ (\c -> (0x5C,(0x75,(0x30,(0x30,toFallback c))))) >$<
        P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8

    toFallback char =
        if w < 0x10
        then (0x30, if w < 0x0a then w + 0x30 else w + 0x57)
        else (0x31, if w < 0x1a then w + 0x20 else w + 0x47)
      where
        w = fromIntegral (Char.ord char)



-- GENERATE MAIN


generateMain :: Mode.Mode -> ModuleName.Canonical -> Opt.Main -> JS.Expr
generateMain mode home main =
  case main of
    Opt.Static ->
      JS.Ref (JsName.fromKernel Module.kernel_vdom [N.ascii|init|])
        # JS.Ref (JsName.fromGlobal home N.main)
        # JS.Int 0
        # JS.Int 0

    Opt.Dynamic msgType decoder ->
      JS.Ref (JsName.fromGlobal home N.main)
        # generateJsExpr mode decoder
        # toDebugMetadata mode msgType


(#) :: JS.Expr -> JS.Expr -> JS.Expr
(#) func arg =
  JS.Call func [arg]


toDebugMetadata :: Mode.Mode -> Can.Type -> JS.Expr
toDebugMetadata mode msgType =
  case mode of
    Mode.Prod _ ->
      JS.Int 0

    Mode.Dev Nothing ->
      JS.Int 0

    Mode.Dev (Just interfaces) ->
      JS.Json $ Encode.object $
        [ "versions" ==> Encode.object [ "elm" ==> V.encode V.compiler ]
        , "types"    ==> Type.encodeMetadata (Extract.fromMsg interfaces msgType)
        ]
