{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Expression
  ( generate
  , generateCtor
  , generateFunction
  , generateMain
  , Code
  , codeToExpr
  , codeToStmtList
  )
  where


import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as Name
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- ENVIRONMENT


type Mode = Name.Mode



-- EXPRESSIONS


generateJsExpr :: Mode -> Opt.Expr -> JS.Expr
generateJsExpr mode expression =
  codeToExpr (generate mode expression)


generate :: Mode -> Opt.Expr -> Code
generate mode expression =
  case expression of
    Opt.Chr char ->
      JsExpr $
        case mode of
          Name.Debug _ ->
            JS.Call toChar [ JS.String (Text.encodeUtf8Builder char) ]

          Name.Prod _ _ ->
            JS.String (Text.encodeUtf8Builder char)

    Opt.Str string ->
      JsExpr $ JS.String (Text.encodeUtf8Builder string)

    Opt.Int int ->
      JsExpr $ JS.Int int

    Opt.Float float ->
      JsExpr $ JS.Float float

    Opt.VarLocal name ->
      JsExpr $ JS.Ref (Name.fromLocal name)

    Opt.VarGlobal (Opt.Global home name) ->
      JsExpr $ JS.Ref (Name.fromGlobal home name)

    Opt.VarCycle home name ->
      JsExpr $ JS.Call (JS.Ref (Name.fromCycle home name)) []

    Opt.VarDebug name home region unhandledValueName ->
      JsExpr $ generateDebug name home region unhandledValueName

    Opt.VarKernel home name ->
      JsExpr $ JS.Ref (Name.fromKernel home name)

    Opt.List entries ->
      JsExpr $
        JS.Call
          (JS.Ref (Name.fromKernel N.list "fromArray"))
          [ JS.Array $ map (generateJsExpr mode) entries
          ]

    Opt.Function args body ->
      generateFunction (map Name.fromLocal args) (generate mode body)

    Opt.Call func args ->
      JsExpr $ generateCall mode func args

    Opt.TailCall name args ->
      JsBlock $ generateTailCall mode name args

    Opt.If branches final ->
      generateIf mode branches final

    Opt.Let def body ->
      JsBlock $
        generateDef mode def : codeToStmtList (generate mode body)

    Opt.Case label root decider jumps ->
      JsBlock $ generateCase mode label root decider jumps

    Opt.Index expr index ->
      JsExpr $ JS.Access (generateJsExpr mode expr) (Name.fromInt index)

    Opt.Accessor field ->
      JsExpr $ JS.Function Nothing [Name.dollar]
        [ JS.Return $ Just $
            JS.Access (JS.Ref Name.dollar) (Name.fromField mode field)
        ]

    Opt.Access record field ->
      JsExpr $ JS.Access (generateJsExpr mode record) (Name.fromField mode field)

    Opt.Update record fields ->
      JsExpr $
        JS.Call (JS.Ref (Name.fromKernel N.utils "update"))
          [ generateJsExpr mode record
          , generateRecord mode fields
          ]

    Opt.Record fields ->
      JsExpr $ generateRecord mode fields

    Opt.Unit ->
      JsExpr $ JS.Ref (Name.fromKernel N.utils "Tuple0")

    Opt.Tuple a b maybeC ->
      JsExpr $
        case maybeC of
          Nothing ->
            JS.Call (JS.Ref (Name.fromKernel N.utils "Tuple2"))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              ]

          Just c ->
            JS.Call (JS.Ref (Name.fromKernel N.utils "Tuple3"))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              , generateJsExpr mode c
              ]

    Opt.Shader src ->
      let string = JS.String (Text.encodeUtf8Builder src) in
      JsExpr $ JS.Object [ ( Name.fromLocal "src", string ) ]



-- CODE CHUNKS


data Code
    = JsExpr JS.Expr
    | JsBlock [JS.Stmt]


codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr expr ->
      expr

    JsBlock stmts ->
      JS.Call (JS.Function Nothing [] stmts) []


codeToStmtList :: Code -> [JS.Stmt]
codeToStmtList code =
  case code of
    JsExpr expr ->
        [ JS.Return (Just expr) ]

    JsBlock stmts ->
        stmts


codeToStmt :: Code -> JS.Stmt
codeToStmt code =
  case code of
    JsExpr expr ->
        JS.Return (Just expr)

    JsBlock [stmt] ->
        stmt

    JsBlock stmts ->
        JS.Block stmts



-- CHARS


{-# NOINLINE toChar #-}
toChar :: JS.Expr
toChar =
  JS.Ref (Name.fromKernel N.utils "chr")



-- CTOR


generateCtor :: Mode -> N.Name -> Index.ZeroBased -> Int -> ([Name.Name], Code)
generateCtor mode name index arity =
  let
    argNames  = map Name.fromInt [1 .. arity]
    argFields = map (\n -> (n, JS.Ref n)) argNames

    tag =
      case mode of
        Name.Debug _ ->
          ( Name.dollar, JS.String (N.toBuilder name) )

        Name.Prod _ _ ->
          ( Name.dollar, JS.Int (Index.toZeroBased index) )
  in
    ( argNames, JsExpr (JS.Object (tag:argFields)) )



-- RECORDS


generateRecord :: Mode -> Map.Map N.Name Opt.Expr -> JS.Expr
generateRecord mode fields =
  let
    toPair (field, value) =
      (Name.fromField mode field, generateJsExpr mode value)
  in
  JS.Object (map toPair (Map.toList fields))



-- DEBUG


generateDebug :: N.Name -> ModuleName.Canonical -> R.Region -> Maybe N.Name -> JS.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= "crash" then
    JS.Ref (Name.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (Name.fromKernel N.utils "crash")) $
          [ JS.String (N.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (Name.fromKernel N.utils "crashCase")) $
          [ JS.String (N.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (Name.fromLocal valueName)
          ]


regionToJsExpr :: R.Region -> JS.Expr
regionToJsExpr (R.Region start end) =
  JS.Object
    [ ( Name.fromLocal "start", positionToJsExpr start )
    , ( Name.fromLocal "end", positionToJsExpr end )
    ]


positionToJsExpr :: R.Position -> JS.Expr
positionToJsExpr (R.Position line column) =
  JS.Object
    [ ( Name.fromLocal "line", JS.Int line )
    , ( Name.fromLocal "column", JS.Int column )
    ]



-- FUNCTION


generateFunction :: [Name.Name] -> Code -> Code
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
  IntMap.fromList
    [ (2, JS.Ref (Name.fromLocal "F2"))
    , (3, JS.Ref (Name.fromLocal "F3"))
    , (4, JS.Ref (Name.fromLocal "F4"))
    , (5, JS.Ref (Name.fromLocal "F5"))
    , (6, JS.Ref (Name.fromLocal "F6"))
    , (7, JS.Ref (Name.fromLocal "F7"))
    , (8, JS.Ref (Name.fromLocal "F8"))
    , (9, JS.Ref (Name.fromLocal "F9"))
    ]



-- CALLS


generateCall :: Mode -> Opt.Expr -> [Opt.Expr] -> JS.Expr
generateCall mode func args =
  case func of
    Opt.VarGlobal global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      generateCoreCall mode global args

    _ ->
      generateNormalCall
        (generateJsExpr mode func)
        (map (generateJsExpr mode) args)


generateNormalCall' :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateNormalCall' home name args =
  generateNormalCall (JS.Ref (Name.fromGlobal home name)) args


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
  IntMap.fromList
    [ (2, JS.Ref (Name.fromLocal "A2"))
    , (3, JS.Ref (Name.fromLocal "A3"))
    , (4, JS.Ref (Name.fromLocal "A4"))
    , (5, JS.Ref (Name.fromLocal "A5"))
    , (6, JS.Ref (Name.fromLocal "A6"))
    , (7, JS.Ref (Name.fromLocal "A7"))
    , (8, JS.Ref (Name.fromLocal "A8"))
    , (9, JS.Ref (Name.fromLocal "A9"))
    ]



-- CORE CALLS


generateCoreCall :: Mode -> Opt.Global -> [Opt.Expr] -> JS.Expr
generateCoreCall mode (Opt.Global home@(ModuleName.Canonical _ moduleName) name) args =
  if moduleName == N.basics then
    generateBasicsCall mode home name args

  else if moduleName == N.bitwise then
    generateBitwiseCall home name (map (generateJsExpr mode) args)

  else if moduleName == N.tuple then
    generateTupleCall home name (map (generateJsExpr mode) args)

  else
    generateNormalCall' home name (map (generateJsExpr mode) args)


generateTupleCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateTupleCall home name args =
  case args of
    [value] ->
      case name of
        "first"  -> JS.Access value (Name.fromLocal "a")
        "second" -> JS.Access value (Name.fromLocal "b")
        _        -> generateNormalCall' home name args

    _ ->
      generateNormalCall' home name args


generateBitwiseCall :: ModuleName.Canonical -> N.Name -> [JS.Expr] -> JS.Expr
generateBitwiseCall home name args =
  case args of
    [arg] ->
      case name of
        "complement" -> JS.Prefix JS.PrefixBNot arg
        _            -> generateNormalCall' home name args

    [left,right] ->
      case name of
        "and"            -> JS.Infix JS.OpBAnd     left right
        "or"             -> JS.Infix JS.OpBOr      left right
        "xor"            -> JS.Infix JS.OpBXor     left right
        "shiftLeftBy"    -> JS.Infix JS.OpLShift   right left
        "shiftRightBy"   -> JS.Infix JS.OpSpRShift right left
        "shiftRightZfBy" -> JS.Infix JS.OpZfRShift right left
        _                -> generateNormalCall' home name args

    _ ->
      generateNormalCall' home name args


generateBasicsCall :: Mode -> ModuleName.Canonical -> N.Name -> [Opt.Expr] -> JS.Expr
generateBasicsCall mode home name args =
  case args of
    [elmArg] ->
      let arg = generateJsExpr mode elmArg in
      case name of
        "not"      -> JS.Prefix JS.PrefixLNot arg
        "toFloat"  -> arg
        "truncate" -> JS.Infix JS.OpBOr arg (JS.Int 0)
        _          -> generateNormalCall' home name [arg]

    [elmLeft, elmRight] ->
      case name of
        "composeL" -> decomposeL mode elmLeft [elmRight]
        "composeR" -> decomposeR mode [elmLeft] elmRight
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
            "idiv" -> JS.Infix JS.OpBOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
            "eq"   -> equal left right
            "neq"  -> notEqual left right
            "lt"   -> cmp JS.OpLT  JS.OpLT   0  left right
            "gt"   -> cmp JS.OpGT  JS.OpGT   0  left right
            "le"   -> cmp JS.OpLEq JS.OpLT   1  left right
            "ge"   -> cmp JS.OpGEq JS.OpGT (-1) left right
            "or"   -> JS.Infix JS.OpLOr         left right
            "and"  -> JS.Infix JS.OpLAnd        left right
            "xor"  -> JS.Infix JS.OpStrictNEq   left right
            _      -> generateNormalCall' home name [left, right]

    _ ->
      generateNormalCall' home name (map (generateJsExpr mode) args)


equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (Name.fromKernel N.utils "eq")) [left, right]


notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixLNot $
      JS.Call (JS.Ref (Name.fromKernel N.utils "eq")) [left, right]


cmp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (Name.fromKernel N.utils "cmp")) [left, right])
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


decomposeL :: Mode -> Opt.Expr -> [Opt.Expr] -> JS.Expr
decomposeL mode expr funcs =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global home "composeL")) [left, func]
      | home == ModuleName.basics ->
          decomposeL mode left (func:funcs)

    _ ->
      generateJsExpr mode $
        Opt.Function [N.dollar] (foldr apply (Opt.VarLocal N.dollar) (expr:funcs))


decomposeR :: Mode -> [Opt.Expr] -> Opt.Expr -> JS.Expr
decomposeR mode funcs expr =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global home "composeR")) [func, right]
      | home == ModuleName.basics ->
          decomposeR mode (func:funcs) right

    _ ->
      generateJsExpr mode $
        Opt.Function [N.dollar] (foldr apply (Opt.VarLocal N.dollar) (expr:funcs))


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field ->
      Opt.Access value field

    Opt.Call f args ->
      Opt.Call f (args ++ [value])

    _ ->
      Opt.Call func [value]


append :: Mode -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode left right =
  let seqs = generateJsExpr mode left : toSeqs mode right in
  if any isStringLiteral seqs then
    foldr1 (JS.Infix JS.OpAdd) seqs
  else
    foldr1 jsAppend seqs


jsAppend :: JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
  JS.Call (JS.Ref (Name.fromKernel N.utils "ap")) [a, b]


toSeqs :: Mode -> Opt.Expr -> [JS.Expr]
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
      JS.Prefix JS.PrefixLNot right

    JS.Bool bool ->
      if bool then right else JS.Prefix JS.PrefixLNot right

    _ ->
      case right of
        JS.Int 0 ->
          JS.Prefix JS.PrefixLNot left

        JS.Bool bool ->
          if bool then left else JS.Prefix JS.PrefixLNot left

        _ ->
          JS.Infix JS.OpStrictEq left right


strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int 0 ->
      right

    JS.Bool bool ->
      if bool then JS.Prefix JS.PrefixLNot right else right

    _ ->
      case right of
        JS.Int 0 ->
          left

        JS.Bool bool ->
          if bool then JS.Prefix JS.PrefixLNot left else left

        _ ->
          JS.Infix JS.OpStrictNEq left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
generateTailCall :: Mode -> N.Name -> [(N.Name, Opt.Expr)] -> [JS.Stmt]
generateTailCall mode name args =
  let
    toTempVars (argName, arg) =
      ( Name.makeTemp argName, Just (generateJsExpr mode arg) )

    toRealVars (argName, _) =
      JS.ExprStmt $
        JS.Assign (JS.LRef (Name.fromLocal argName)) (JS.Ref (Name.makeTemp argName))
  in
  JS.Var (map toTempVars args)
  : map toRealVars args
  ++ [ JS.Continue (Just (Name.fromLocal name)) ]



-- DEFINITIONS


generateDef :: Mode -> Opt.Def -> JS.Stmt
generateDef mode def =
  case def of
    Opt.Def name body ->
      JS.Var [ (Name.fromLocal name, Just (generateJsExpr mode body)) ]

    Opt.TailDef name argNames body ->
      let
        function =
          JS.Function Nothing (map Name.fromLocal argNames)
            [ JS.Labelled (Name.fromLocal name) $
                JS.While (JS.Bool True) $
                  codeToStmt $ generate mode body
            ]
      in
      JS.Var [ (Name.fromLocal name, Just function) ]



-- GENERATE IFS


generateIf :: Mode -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
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


generateCase :: Mode -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
generateCase mode label root decider jumps =
  foldr (goto mode label) (generateDecider mode label root decider) jumps


goto :: Mode -> N.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (Name.makeLabel label index)
        (JS.While (JS.Bool True) (JS.Block stmts))
  in
  labeledDeciderStmt : codeToStmtList (generate mode branch)


generateDecider :: Mode -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
generateDecider mode label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode branch)

    Opt.Leaf (Opt.Jump index) ->
      [ JS.Break (Just (Name.makeLabel label index)) ]

    Opt.Chain testChain success failure ->
      let
        makeTest (path, test) =
          strictEq
            (pathToTestableExpr mode root path test)
            (testToExpr mode test)
      in
      [ JS.IfStmt
          (List.foldl1' (JS.Infix JS.OpLAnd) (map makeTest testChain))
          (JS.Block $ generateDecider mode label root success)
          (JS.Block $ generateDecider mode label root failure)
      ]

    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (pathToTestableExpr mode root path (fst (head edges)))
          ( foldr
              (addCase mode label root)
              [ JS.Default (generateDecider mode label root fallback) ]
              edges
          )
      ]


addCase :: Mode -> N.Name -> N.Name -> (DT.Test, Opt.Decider Opt.Choice) -> [JS.Case] -> [JS.Case]
addCase mode label root (test, subTree) cases =
  let stmts = generateDecider mode label root subTree in
  JS.Case (testToExpr mode test) stmts : cases


testToExpr :: Mode -> DT.Test -> JS.Expr
testToExpr mode test =
  case test of
    DT.IsCtor _ tag index ->
      case mode of
        Name.Debug _ ->
          JS.String (N.toBuilder tag)

        Name.Prod _ _ ->
          JS.Int (Index.toZeroBased index)

    DT.IsInt int ->
      JS.Int int

    DT.IsChr char ->
      JS.String (Text.encodeUtf8Builder char)

    DT.IsStr string ->
      JS.String (Text.encodeUtf8Builder string)


pathToTestableExpr :: Mode -> N.Name -> DT.Path -> DT.Test -> JS.Expr
pathToTestableExpr mode root path exampleTest =
  let
    expr =
      pathToJsExpr mode (JS.Ref (Name.fromLocal root)) path
  in
  case exampleTest of
    DT.IsCtor _ _ _ ->
      JS.Access expr Name.dollar

    DT.IsInt _ ->
      expr

    DT.IsStr _ ->
      expr

    DT.IsChr _ ->
      case mode of
        Name.Debug _ ->
          JS.Call (JS.Access expr (Name.fromLocal "valueOf")) []

        Name.Prod _ _ ->
          expr


pathToJsExpr :: Mode -> JS.Expr -> DT.Path -> JS.Expr
pathToJsExpr mode expr path =
  case path of
    DT.Index index subpath ->
      pathToJsExpr mode (JS.Access expr (Name.fromInt index)) subpath

    DT.Field field subpath ->
        pathToJsExpr mode (JS.Access expr (Name.fromField mode field)) subpath

    DT.Empty ->
        expr

    DT.Alias ->
        expr



-- GENERATE MAIN


generateMain :: Mode -> I.Interfaces -> ModuleName.Canonical -> [N.Name] -> Opt.Main -> JS.Expr
generateMain mode interfaces home segments main =
  case main of
    Opt.Static ->
      JS.Ref (Name.fromKernel N.browser "staticPage")
        # JS.Ref (Name.fromGlobal home "main")
        # List.foldl' addDot elm segments

    Opt.Dynamic decoder msgType ->
      JS.Ref (Name.fromGlobal home "main")
        # List.foldl' addDot elm segments
        # generateJsExpr mode decoder
        # JS.Json (Type.encodeMetadata (Extract.fromMsg interfaces msgType))


(#) :: JS.Expr -> JS.Expr -> JS.Expr
(#) func arg =
  JS.Call func [arg]


addDot :: JS.Expr -> N.Name -> JS.Expr
addDot root name =
  JS.Index root (JS.String (N.toBuilder name))


{-# NOINLINE elm #-}
elm :: JS.Expr
elm =
  JS.Ref (Name.fromLocal "Elm")
