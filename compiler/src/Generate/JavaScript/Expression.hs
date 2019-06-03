{-# LANGUAGE OverloadedStrings #-}
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


import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
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
    Opt.Bool bool ->
      JsExpr $ JS.Bool bool

    Opt.Chr char ->
      JsExpr $
        case mode of
          Mode.Dev _ ->
            JS.Call toChar [ JS.String (Utf8.toBuilder char) ]

          Mode.Prod _ ->
            JS.String (Utf8.toBuilder char)

    Opt.Str string ->
      JsExpr $ JS.String (Utf8.toBuilder string)

    Opt.Int int ->
      JsExpr $ JS.Int int

    Opt.Float float ->
      JsExpr $ JS.Float (Utf8.toBuilder float)

    Opt.VarLocal name ->
      JsExpr $ JS.Ref (JsName.fromLocal name)

    Opt.VarGlobal (Opt.Global home name) ->
      JsExpr $ JS.Ref (JsName.fromGlobal home name)

    Opt.VarEnum (Opt.Global home name) index ->
      case mode of
        Mode.Dev _ ->
          JsExpr $ JS.Ref (JsName.fromGlobal home name)

        Mode.Prod _ ->
          JsExpr $ JS.Int (Index.toMachine index)

    Opt.VarBox (Opt.Global home name) ->
      JsExpr $ JS.Ref $
        case mode of
          Mode.Dev _ -> JsName.fromGlobal home name
          Mode.Prod _ -> JsName.fromGlobal ModuleName.basics Name.identity

    Opt.VarCycle home name ->
      JsExpr $ JS.Call (JS.Ref (JsName.fromCycle home name)) []

    Opt.VarDebug name home region unhandledValueName ->
      JsExpr $ generateDebug name home region unhandledValueName

    Opt.VarKernel home name ->
      JsExpr $ JS.Ref (JsName.fromKernel home name)

    Opt.List entries ->
      case entries of
        [] ->
          JsExpr $ JS.Ref (JsName.fromKernel Name.list "Nil")

        _ ->
          JsExpr $
            JS.Call
              (JS.Ref (JsName.fromKernel Name.list "fromArray"))
              [ JS.Array $ map (generateJsExpr mode) entries
              ]

    Opt.Function args body ->
      generateFunction (map JsName.fromLocal args) (generate mode body)

    Opt.Call func args ->
      JsExpr $ generateCall mode func args

    Opt.TailCall name args ->
      JsBlock $ generateTailCall mode name args

    Opt.If branches final ->
      generateIf mode branches final

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
        JS.Call (JS.Ref (JsName.fromKernel Name.utils "update"))
          [ generateJsExpr mode record
          , generateRecord mode fields
          ]

    Opt.Record fields ->
      JsExpr $ generateRecord mode fields

    Opt.Unit ->
      case mode of
        Mode.Dev _ ->
          JsExpr $ JS.Ref (JsName.fromKernel Name.utils "Tuple0")

        Mode.Prod _ ->
          JsExpr $ JS.Int 0

    Opt.Tuple a b maybeC ->
      JsExpr $
        case maybeC of
          Nothing ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple2"))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              ]

          Just c ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple3"))
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
        [ ( JsName.fromLocal "src", JS.String (Shader.toJsStringBuilder src) )
        , ( JsName.fromLocal "attributes", toTranslationObject attributes )
        , ( JsName.fromLocal "uniforms", toTranslationObject uniforms )
        ]



-- CODE CHUNKS


data Code
    = JsExpr JS.Expr
    | JsBlock [JS.Stmt]


codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr expr ->
      expr

    JsBlock [ JS.Return expr ] ->
      expr

    JsBlock stmts ->
      JS.Call (JS.Function Nothing [] stmts) []


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
  JS.Ref (JsName.fromKernel Name.utils "chr")



-- CTOR


generateCtor :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

    ctorTag =
      case mode of
        Mode.Dev _ -> JS.String (Name.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)
  in
  generateFunction argNames $ JsExpr $ JS.Object $
    (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames


ctorToInt :: ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
  if home == ModuleName.dict && name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin" then
    0 - Index.toHuman index
  else
    Index.toMachine index



-- RECORDS


generateRecord :: Mode.Mode -> Map.Map Name.Name Opt.Expr -> JS.Expr
generateRecord mode fields =
  let
    toPair (field, value) =
      (generateField mode field, generateJsExpr mode value)
  in
  JS.Object (map toPair (Map.toList fields))


generateField :: Mode.Mode -> Name.Name -> JsName.Name
generateField mode name =
  case mode of
    Mode.Dev _ ->
      JsName.fromLocal name

    Mode.Prod fields ->
      fields ! name




-- DEBUG


generateDebug :: Name.Name -> ModuleName.Canonical -> A.Region -> Maybe Name.Name -> JS.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= "todo" then
    JS.Ref (JsName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todo")) $
          [ JS.String (Name.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todoCase")) $
          [ JS.String (Name.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (JsName.fromLocal valueName)
          ]


regionToJsExpr :: A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ ( JsName.fromLocal "start", positionToJsExpr start )
    , ( JsName.fromLocal "end", positionToJsExpr end )
    ]


positionToJsExpr :: A.Position -> JS.Expr
positionToJsExpr (A.Position line column) =
  JS.Object
    [ ( JsName.fromLocal "line", JS.Int (fromIntegral line) )
    , ( JsName.fromLocal "column", JS.Int (fromIntegral column) )
    ]



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
        Mode.Dev _ ->
          generateCallHelp mode func args

        Mode.Prod _ ->
          case args of
            [arg] ->
              generateJsExpr mode arg

            _ ->
              generateCallHelp mode func args

    _ ->
      generateCallHelp mode func args


generateCallHelp :: Mode.Mode -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCallHelp mode func args =
  generateNormalCall
    (generateJsExpr mode func)
    (map (generateJsExpr mode) args)


generateGlobalCall :: ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateGlobalCall home name args =
  generateNormalCall (JS.Ref (JsName.fromGlobal home name)) args


generateNormalCall :: JS.Expr -> [JS.Expr] -> JS.Expr
generateNormalCall func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper ->
      JS.Call helper (func:args)

    Nothing ->
      List.foldl' (\f a -> JS.Call f [a]) func args


{-# NOINLINE callHelpers #-}
callHelpers :: IntMap.IntMap JS.Expr
callHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeA n))) [2..9]



-- CORE CALLS


generateCoreCall :: Mode.Mode -> Opt.Global -> [Opt.Expr] -> JS.Expr
generateCoreCall mode (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
  if moduleName == Name.basics then
    generateBasicsCall mode home name args

  else if moduleName == Name.bitwise then
    generateBitwiseCall home name (map (generateJsExpr mode) args)

  else if moduleName == Name.tuple then
    generateTupleCall home name (map (generateJsExpr mode) args)

  else if moduleName == Name.jsArray then
    generateJsArrayCall home name (map (generateJsExpr mode) args)

  else
    generateGlobalCall home name (map (generateJsExpr mode) args)


generateTupleCall :: ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateTupleCall home name args =
  case args of
    [value] ->
      case name of
        "first"  -> JS.Access value (JsName.fromLocal "a")
        "second" -> JS.Access value (JsName.fromLocal "b")
        _        -> generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateJsArrayCall :: ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateJsArrayCall home name args =
  case args of
    [entry]        | name == "singleton" -> JS.Array [entry]
    [index, array] | name == "unsafeGet" -> JS.Index array index
    _                                    -> generateGlobalCall home name args


generateBitwiseCall :: ModuleName.Canonical -> Name.Name -> [JS.Expr] -> JS.Expr
generateBitwiseCall home name args =
  case args of
    [arg] ->
      case name of
        "complement" -> JS.Prefix JS.PrefixComplement arg
        _            -> generateGlobalCall home name args

    [left,right] ->
      case name of
        "and"            -> JS.Infix JS.OpBitwiseAnd left right
        "or"             -> JS.Infix JS.OpBitwiseOr  left right
        "xor"            -> JS.Infix JS.OpBitwiseXor left right
        "shiftLeftBy"    -> JS.Infix JS.OpLShift     right left
        "shiftRightBy"   -> JS.Infix JS.OpSpRShift   right left
        "shiftRightZfBy" -> JS.Infix JS.OpZfRShift   right left
        _                -> generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateBasicsCall :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> [Opt.Expr] -> JS.Expr
generateBasicsCall mode home name args =
  case args of
    [elmArg] ->
      let arg = generateJsExpr mode elmArg in
      case name of
        "not"      -> JS.Prefix JS.PrefixNot arg
        "negate"   -> JS.Prefix JS.PrefixNegate arg
        "toFloat"  -> arg
        "truncate" -> JS.Infix JS.OpBitwiseOr arg (JS.Int 0)
        _          -> generateGlobalCall home name [arg]

    [elmLeft, elmRight] ->
      case name of
        -- NOTE: removed "composeL" and "composeR" because of this issue:
        -- https://github.com/elm/compiler/issues/1722
        "append"   -> append mode elmLeft elmRight
        "apL"      -> generateJsExpr mode $ apply elmLeft elmRight
        "apR"      -> generateJsExpr mode $ apply elmRight elmLeft
        _ ->
          let
            left = generateJsExpr mode elmLeft
            right = generateJsExpr mode elmRight
          in
          case name of
            "add"  -> JS.Infix JS.OpAdd left right
            "sub"  -> JS.Infix JS.OpSub left right
            "mul"  -> JS.Infix JS.OpMul left right
            "fdiv" -> JS.Infix JS.OpDiv left right
            "idiv" -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
            "eq"   -> equal left right
            "neq"  -> notEqual left right
            "lt"   -> cmp JS.OpLt JS.OpLt   0  left right
            "gt"   -> cmp JS.OpGt JS.OpGt   0  left right
            "le"   -> cmp JS.OpLe JS.OpLt   1  left right
            "ge"   -> cmp JS.OpGe JS.OpGt (-1) left right
            "or"   -> JS.Infix JS.OpOr  left right
            "and"  -> JS.Infix JS.OpAnd left right
            "xor"  -> JS.Infix JS.OpNe  left right
            "remainderBy" -> JS.Infix JS.OpMod right left
            _      -> generateGlobalCall home name [left, right]

    _ ->
      generateGlobalCall home name (map (generateJsExpr mode) args)


equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixNot $
      JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


cmp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (JsName.fromKernel Name.utils "cmp")) [left, right])
      (JS.Int backupInt)


isLiteral :: JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.String _ ->
      True

    JS.Float _ ->
      True

    JS.Int _ ->
      True

    JS.Bool _ ->
      True

    _ ->
      False


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field ->
      Opt.Access value field

    Opt.Call f args ->
      Opt.Call f (args ++ [value])

    _ ->
      Opt.Call func [value]


append :: Mode.Mode -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode left right =
  let seqs = generateJsExpr mode left : toSeqs mode right in
  if any isStringLiteral seqs then
    foldr1 (JS.Infix JS.OpAdd) seqs
  else
    foldr1 jsAppend seqs


jsAppend :: JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
  JS.Call (JS.Ref (JsName.fromKernel Name.utils "ap")) [a, b]


toSeqs :: Mode.Mode -> Opt.Expr -> [JS.Expr]
toSeqs mode expr =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global home "append")) [left, right]
      | home == ModuleName.basics ->
          generateJsExpr mode left : toSeqs mode right

    _ ->
      [generateJsExpr mode expr]


isStringLiteral :: JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.String _ ->
      True

    _ ->
      False



-- SIMPLIFY INFIX OPERATORS


strictEq :: JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot right

    JS.Bool bool ->
      if bool then right else JS.Prefix JS.PrefixNot right

    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot left

        JS.Bool bool ->
          if bool then left else JS.Prefix JS.PrefixNot left

        _ ->
          JS.Infix JS.OpEq left right


strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int 0 ->
      JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)

    JS.Bool bool ->
      if bool then JS.Prefix JS.PrefixNot right else right

    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)

        JS.Bool bool ->
          if bool then JS.Prefix JS.PrefixNot left else left

        _ ->
          JS.Infix JS.OpNe left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
generateTailCall :: Mode.Mode -> Name.Name -> [(Name.Name, Opt.Expr)] -> [JS.Stmt]
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


generateTailDef :: Mode.Mode -> Name.Name -> [Name.Name] -> Opt.Expr -> Code
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
    Opt.Index index subPath ->
      JS.Access (generatePath mode subPath) (JsName.fromIndex index)

    Opt.Root name ->
      JS.Ref (JsName.fromLocal name)

    Opt.Field field subPath ->
      JS.Access (generatePath mode subPath) (generateField mode field)

    Opt.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (generatePath mode subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          generatePath mode subPath



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
    JsExpr _ -> False


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


generateCase :: Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
generateCase mode label root decider jumps =
  foldr (goto mode label) (generateDecider mode label root decider) jumps


goto :: Mode.Mode -> Name.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (JsName.makeLabel label index)
        (JS.While (JS.Bool True) (JS.Block stmts))
  in
  labeledDeciderStmt : codeToStmtList (generate mode branch)


generateDecider :: Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
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


generateIfTest :: Mode.Mode -> Name.Name -> (DT.Path, DT.Test) -> JS.Expr
generateIfTest mode root (path, test) =
  let
    value = pathToJsExpr mode root path
  in
  case test of
    DT.IsCtor home name index _ opts ->
      let
        tag =
          case mode of
            Mode.Dev _ -> JS.Access value JsName.dollar
            Mode.Prod _ ->
              case opts of
                Can.Normal -> JS.Access value JsName.dollar
                Can.Enum   -> value
                Can.Unbox  -> value
      in
      strictEq tag $
        case mode of
          Mode.Dev _ -> JS.String (Name.toBuilder name)
          Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsBool True ->
      value

    DT.IsBool False ->
      JS.Prefix JS.PrefixNot value

    DT.IsInt int ->
      strictEq value (JS.Int int)

    DT.IsChr char ->
      strictEq (JS.String (Utf8.toBuilder char)) $
        case mode of
          Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []
          Mode.Prod _ -> value

    DT.IsStr string ->
      strictEq value (JS.String (Utf8.toBuilder string))

    DT.IsCons ->
      JS.Access value (JsName.fromLocal "b")

    DT.IsNil ->
      JS.Prefix JS.PrefixNot $
        JS.Access value (JsName.fromLocal "b")

    DT.IsTuple ->
      error "COMPILER BUG - there should never be tests on a tuple"



generateCaseBranch :: Mode.Mode -> Name.Name -> Name.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
generateCaseBranch mode label root (test, subTree) =
  JS.Case
    (generateCaseValue mode test)
    (generateDecider mode label root subTree)


generateCaseValue :: Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
  case test of
    DT.IsCtor home name index _ _ ->
      case mode of
        Mode.Dev _ -> JS.String (Name.toBuilder name)
        Mode.Prod _ -> JS.Int (ctorToInt home name index)

    DT.IsInt int ->
      JS.Int int

    DT.IsChr char ->
      JS.String (Utf8.toBuilder char)

    DT.IsStr string ->
      JS.String (Utf8.toBuilder string)

    DT.IsBool _ ->
      error "COMPILER BUG - there should never be three tests on a boolean"

    DT.IsCons ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      error "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest :: Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
  let
    value = pathToJsExpr mode root path
  in
  case exampleTest of
    DT.IsCtor home name _ _ opts ->
      if name == Name.bool && home == ModuleName.basics then
        value
      else
        case mode of
          Mode.Dev _ ->
            JS.Access value JsName.dollar

          Mode.Prod _ ->
            case opts of
              Can.Normal ->
                JS.Access value JsName.dollar

              Can.Enum ->
                value

              Can.Unbox ->
                value

    DT.IsInt _ ->
      value

    DT.IsStr _ ->
      value

    DT.IsChr _ ->
      case mode of
        Mode.Dev _ ->
          JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []

        Mode.Prod _ ->
          value

    DT.IsBool _ ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsCons ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      error "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      error "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr :: Mode.Mode -> Name.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index index subPath ->
      JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex index)

    DT.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          pathToJsExpr mode root subPath

    DT.Empty ->
      JS.Ref (JsName.fromLocal root)



-- GENERATE MAIN


generateMain :: Mode.Mode -> ModuleName.Canonical -> Opt.Main -> JS.Expr
generateMain mode home main =
  case main of
    Opt.Static ->
      JS.Ref (JsName.fromKernel Name.virtualDom "init")
        # JS.Ref (JsName.fromGlobal home "main")
        # JS.Int 0
        # JS.Int 0

    Opt.Dynamic msgType decoder ->
      JS.Ref (JsName.fromGlobal home "main")
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
