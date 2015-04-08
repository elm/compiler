module Generate.JavaScript.Port (inbound, outbound, task) where

import qualified Data.List as List
import Language.ECMAScript3.Syntax

import AST.PrettyPrint (pretty)
import AST.Type as T
import qualified AST.Variable as Var
import Generate.JavaScript.Helpers


-- TASK

task :: String -> Expression () -> T.PortType var -> Expression ()
task name expr portType =
  case portType of
    T.Normal _ ->
        _Task "perform" `call` [ expr ]

    T.Signal _ _ ->
        _Task "performSignal" `call` [ string name, expr ]



-- HELPERS

data JSType
    = JSNumber
    | JSBoolean
    | JSString
    | JSArray
    | JSObject [String]


typeToString :: JSType -> String
typeToString tipe =
  case tipe of
    JSNumber -> "a number"
    JSBoolean -> "a boolean (true or false)"
    JSString -> "a string"
    JSArray -> "an array"
    JSObject fields ->
      "an object with fields '" ++ List.intercalate "', '" fields ++ "'"


_Array :: String -> Expression ()
_Array functionName =
    useLazy ["Elm","Native","Array"] functionName


_List :: String -> Expression ()
_List functionName =
    useLazy ["Elm","Native","List"] functionName


_Maybe :: String -> Expression ()
_Maybe functionName =
    useLazy ["Elm","Maybe"] functionName


_Port :: String -> Expression ()
_Port functionName =
    useLazy ["Elm","Native","Port"] functionName


_Task :: String -> Expression ()
_Task functionName =
    useLazy ["Elm","Native","Task"] functionName


check :: Expression () -> JSType -> Expression () -> Expression ()
check x jsType continue =
    CondExpr () (jsFold OpLOr checks x) continue throw
  where
    jsFold op checks value =
        foldl1 (InfixExpr () op) (map ($ value) checks)

    throw =
        obj ["_U","badPort"] `call` [ string (typeToString jsType), x ]

    checks =
        case jsType of
          JSNumber  -> [typeof "number"]
          JSBoolean -> [typeof "boolean"]
          JSString  -> [typeof "string", instanceof "String"]
          JSArray   -> [instanceof "Array"]
          JSObject fields ->
              [jsFold OpLAnd (typeof "object" : map member fields)]


-- INBOUND

inbound :: String -> T.PortType Var.Canonical -> Expression ()
inbound name portType =
  case portType of
    T.Normal tipe ->
        _Port "inbound" `call`
            [ string name
            , string (show (pretty tipe))
            , toTypeFunction tipe
            ]

    T.Signal _root arg ->
        _Port "inboundSignal" `call`
            [ string name
            , string (show (pretty arg))
            , toTypeFunction arg
            ]


toTypeFunction :: CanonicalType -> Expression ()
toTypeFunction tipe =
    ["v"] ==> toType tipe (ref "v")


toType :: CanonicalType -> Expression () -> Expression ()
toType tipe x =
    case tipe of
      Lambda _ _ ->
          error "functions should not be allowed through input ports"

      Var _ ->
          error "type variables should not be allowed through input ports"

      Aliased _ args t ->
          toType (dealias args t) x

      Type (Var.Canonical Var.BuiltIn name)
          | name == "Int"    -> from JSNumber
          | name == "Float"  -> from JSNumber
          | name == "Bool"   -> from JSBoolean
          | name == "String" -> from JSString
          where
            from checks = check x checks x

      Type name
          | Var.isJson name ->
              x

          | Var.isTuple name ->
              toTuple [] x

          | otherwise ->
              error "bad type got to foreign input conversion"

      App f args ->
          case f : args of
            Type name : [t]
                | Var.isMaybe name ->
                    CondExpr ()
                        (equal x (NullLit ()))
                        (_Maybe "Nothing")
                        (_Maybe "Just" <| toType t x)

                | Var.isList name ->
                    check x JSArray (_List "fromArray" <| array)

                | Var.isArray name ->
                    check x JSArray (_Array "fromJSArray" <| array)
                where
                  array = DotRef () x (var "map") <| toTypeFunction t

            Type name : ts
                | Var.isTuple name ->
                    toTuple ts x

            _ -> error "bad ADT got to foreign input conversion"

      Record _ (Just _) ->
          error "bad record got to foreign input conversion"

      Record fields Nothing ->
          check x (JSObject (map fst fields)) object
        where
          object = ObjectLit () $ (prop "_", ObjectLit () []) : keys
          keys = map convert fields
          convert (f,t) = (prop f, toType t (DotRef () x (var f)))


toTuple :: [CanonicalType] -> Expression () -> Expression ()
toTuple types x =
    check x JSArray (ObjectLit () fields)
  where
    fields =
        (prop "ctor", ctor) : zipWith convert [0..] types

    ctor =
        string ("_Tuple" ++ show (length types))

    convert n t =
        ( prop ('_':show n)
        , toType t (BracketRef () x (IntLit () n))
        )


-- OUTBOUND

outbound :: String -> Expression () -> T.PortType Var.Canonical -> Expression ()
outbound name expr portType =
  case portType of
    T.Normal tipe ->
        _Port "outbound" `call` [ string name, fromTypeFunction tipe, expr ]

    T.Signal _ arg ->
        _Port "outboundSignal" `call` [ string name, fromTypeFunction arg, expr ]


fromTypeFunction :: CanonicalType -> Expression ()
fromTypeFunction tipe =
    ["v"] ==> fromType tipe (ref "v")


fromType :: CanonicalType -> Expression () -> Expression ()
fromType tipe x =
    case tipe of
      Aliased _ args t ->
          fromType (dealias args t) x

      Lambda _ _
          | numArgs > 1 && numArgs < 10 ->
              func (ref ('A':show numArgs) `call` (x:values))
          | otherwise ->
              func (foldl (<|) x values)
          where
            ts = T.collectLambdas tipe
            numArgs = length ts - 1
            args = map (\n -> '_' : show n) [0..]
            values = zipWith toType (init ts) (map ref args)
            func body =
                function (take numArgs args)
                    [ VarDeclStmt () [VarDecl () (var "_r") (Just body)]
                    , ret (fromType (last ts) (ref "_r"))
                    ]

      Var _ ->
          error "type variables should not be allowed through outputs"

      Type (Var.Canonical Var.BuiltIn name)
          | name `elem` ["Int","Float","Bool","String"] ->
              x

      Type name
          | Var.isJson name -> x
          | Var.isTuple name -> ArrayLit () []
          | otherwise -> error "bad type got to an output"

      App f args ->
          case f : args of
            Type name : [t]
                | Var.isMaybe name ->
                    CondExpr ()
                        (equal (DotRef () x (var "ctor")) (string "Nothing"))
                        (NullLit ())
                        (fromType t (DotRef () x (var "_0")))

                | Var.isArray name ->
                    DotRef () (_Array "toJSArray" <| x) (var "map") <| fromTypeFunction t

                | Var.isList name ->
                    DotRef () (_List "toArray" <| x) (var "map") <| fromTypeFunction t

            Type name : ts
                | Var.isTuple name ->
                    let convert n t = fromType t $ DotRef () x $ var ('_':show n)
                    in  ArrayLit () $ zipWith convert [0..] ts

            _ -> error "bad ADT got to an output"

      Record _ (Just _) ->
          error "bad record got to an output"

      Record fields Nothing ->
          ObjectLit () keys
        where
          keys = map convert fields
          convert (f,t) =
              (PropId () (var f), fromType t (DotRef () x (var f)))
