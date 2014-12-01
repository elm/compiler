module Generate.JavaScript.Ports (incoming, outgoing) where

import qualified Data.List as List
import Generate.JavaScript.Helpers
import AST.Type as T
import qualified AST.Variable as Var
import Language.ECMAScript3.Syntax


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
          JSObject fields -> [jsFold OpLAnd (typeof "object" : map member fields)]


incoming :: CanonicalType -> Expression ()
incoming tipe =
  case tipe of
    Aliased _ t -> incoming t

    App (Type v) [t]
        | Var.isSignal v -> obj ["_P","incomingSignal"] <| incoming t

    _ -> ["v"] ==> inc tipe (ref "v")


inc :: CanonicalType -> Expression () -> Expression ()
inc tipe x =
    case tipe of
      Lambda _ _ -> error "functions should not be allowed through input ports"
      Var _ -> error "type variables should not be allowed through input ports"
      Aliased _ t ->
          inc t x

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
              incomingTuple [] x

          | otherwise ->
              error "bad type got to incoming port generation code"

      App f args ->
          case f : args of
            Type name : [t]
                | Var.isMaybe name ->
                    CondExpr ()
                        (equal x (NullLit ()))
                        (_Maybe "Nothing")
                        (_Maybe "Just" <| inc t x)

                | Var.isList name ->
                    check x JSArray (_List "fromArray" <| array)

                | Var.isArray name ->
                    check x JSArray (_Array "fromJSArray" <| array)
                where
                  array = DotRef () x (var "map") <| incoming t

            Type name : ts
                | Var.isTuple name -> incomingTuple ts x

            _ -> error "bad ADT got to incoming port generation code"

      Record _ (Just _) ->
          error "bad record got to incoming port generation code"

      Record fields Nothing ->
          check x (JSObject (map fst fields)) object
          where
            object = ObjectLit () $ (prop "_", ObjectLit () []) : keys
            keys = map convert fields
            convert (f,t) = (prop f, inc t (DotRef () x (var f)))


incomingTuple :: [CanonicalType] -> Expression () -> Expression ()
incomingTuple types x =
    check x JSArray (ObjectLit () fields)
  where
    fields = (prop "ctor", ctor) : zipWith convert [0..] types

    ctor = string ("_Tuple" ++ show (length types))

    convert n t =
        ( prop ('_':show n)
        , inc t (BracketRef () x (IntLit () n))
        )


outgoing :: CanonicalType -> Expression ()
outgoing tipe =
  case tipe of
    Aliased _ t -> outgoing t

    App (Type v) [t]
        | Var.isSignal v -> obj ["_P","outgoingSignal"] <| outgoing t

    _ -> ["v"] ==> out tipe (ref "v")


out :: CanonicalType -> Expression () -> Expression ()
out tipe x =
    case tipe of
      Aliased _ t -> out t x

      Lambda _ _
          | numArgs > 1 && numArgs < 10 ->
              func (ref ('A':show numArgs) `call` (x:values))
          | otherwise -> func (foldl (<|) x values)
          where
            ts = T.collectLambdas tipe
            numArgs = length ts - 1
            args = map (\n -> '_' : show n) [0..]
            values = zipWith inc (init ts) (map ref args)
            func body =
                function (take numArgs args)
                    [ VarDeclStmt () [VarDecl () (var "_r") (Just body)]
                    , ret (out (last ts) (ref "_r"))
                    ]

      Var _ -> error "type variables should not be allowed through input ports"

      Type (Var.Canonical Var.BuiltIn name)
          | name `elem` ["Int","Float","Bool","String"] -> x

      Type name
          | Var.isJson name -> x
          | Var.isTuple name -> ArrayLit () []
          | otherwise -> error "bad type got to outgoing port generation code"

      App f args ->
          case f : args of
            Type name : [t]
                | Var.isMaybe name ->
                    CondExpr ()
                        (equal (DotRef () x (var "ctor")) (string "Nothing"))
                        (NullLit ())
                        (out t (DotRef () x (var "_0")))

                | Var.isArray name ->
                    DotRef () (_Array "toJSArray" <| x) (var "map") <| outgoing t

                | Var.isList name ->
                    DotRef () (_List "toArray" <| x) (var "map") <| outgoing t

            Type name : ts
                | Var.isTuple name ->
                    let convert n t = out t $ DotRef () x $ var ('_':show n)
                    in  ArrayLit () $ zipWith convert [0..] ts

            _ -> error "bad ADT got to outgoing port generation code"

      Record _ (Just _) ->
          error "bad record got to outgoing port generation code"

      Record fields Nothing ->
          ObjectLit () keys
          where
            keys = map convert fields
            convert (f,t) = (PropId () (var f), out t (DotRef () x (var f)))
