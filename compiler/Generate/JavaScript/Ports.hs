{-# OPTIONS_GHC -W #-}
module Generate.JavaScript.Ports (incoming, outgoing) where

import Generate.JavaScript.Helpers ((<|), (==>), call, equal, function,
                                    instanceof, member, obj, prop, ref, ret,
                                    string, typeof, var)
import qualified Generate.JavaScript.Variable as V
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Language.ECMAScript3.Syntax as JS

data JSType = JSNumber | JSBoolean | JSString | JSArray | JSObject [String]
    deriving Show

check :: JS.Expression () -> JSType -> JS.Expression () -> JS.Expression ()
check x jsType continue =
    JS.CondExpr () (jsFold JS.OpLOr checks x) continue throw
  where
    jsFold op checks value = foldl1 (JS.InfixExpr () op) (map ($value) checks)
    throw = obj ["_E","raise"] <| JS.InfixExpr () JS.OpAdd msg x
    msg = string ("invalid input, expecting " ++ show jsType ++ " but got ")
    checks = case jsType of
               JSNumber  -> [typeof "number"]
               JSBoolean -> [typeof "boolean"]
               JSString  -> [typeof "string", instanceof "String"]
               JSArray   -> [(obj ["_U","isJSArray"] <|)]
               JSObject fields -> [jsFold JS.OpLAnd (typeof "object" : map member fields)]

incoming :: T.CanonicalType -> JS.Expression ()
incoming tipe =
  case tipe of
    T.Aliased _ t -> incoming t

    T.App (T.Type v) [t]
        | Var.isSignal v -> V.value "Native.Ports" "incomingSignal" <| incoming t

    _ -> ["v"] ==> inc tipe (ref "v")

inc :: T.CanonicalType -> JS.Expression () -> JS.Expression ()
inc tipe x =
    case tipe of
      T.Lambda _ _ -> error "functions should not be allowed through input ports"
      T.Var _ -> error "type variables should not be allowed through input ports"
      T.Aliased _ t ->
          inc t x

      T.Type (Var.Canonical Var.BuiltIn name)
          | name == "Int"    -> from JSNumber
          | name == "Float"  -> from JSNumber
          | name == "Bool"   -> from JSBoolean
          | name == "String" -> from JSString
          where
            from checks = check x checks x

      T.Type name
          | Var.isJson name -> V.value "Native.Json" "fromJS" <| x
          | otherwise -> error "bad type got to incoming port generation code"

      T.App f args ->
          case f : args of
            T.Type name : [t]
                | Var.isMaybe name -> JS.CondExpr () (equal x (JS.NullLit ()))
                                                  (V.value "Maybe" "Nothing")
                                                  (V.value "Maybe" "Just" <| inc t x)

                | Var.isList name  -> check x JSArray (obj ["_L","fromArray"] <| array)

                | Var.isArray name -> check x JSArray (obj ["_A","fromJSArray"] <| array)
                where
                  array = JS.DotRef () x (var "map") <| incoming t

            T.Type name : ts
                | Var.isTuple name -> check x JSArray tuple
                where
                  tuple = JS.ObjectLit () $ (prop "ctor", ctor) : values

                  ctor = string ("_Tuple" ++ show (length ts))
                  values = zipWith convert [0..] ts

                  convert n t = ( prop ('_':show n)
                                , inc t (JS.BracketRef () x (JS.IntLit () n))
                                )

            _ -> error "bad ADT got to incoming port generation code"

      T.Record _ (Just _) ->
          error "bad record got to incoming port generation code"

      T.Record fields Nothing ->
          check x (JSObject (map fst fields)) object
          where
            object = JS.ObjectLit () $ (prop "_", JS.ObjectLit () []) : keys
            keys = map convert fields
            convert (f,t) = (prop f, inc t (JS.DotRef () x (var f)))

outgoing :: T.CanonicalType -> JS.Expression ()
outgoing tipe =
  case tipe of
    T.Aliased _ t -> outgoing t

    T.App (T.Type v) [t]
        | Var.isSignal v -> V.value "Native.Ports" "outgoingSignal" <| outgoing t

    _ -> ["v"] ==> out tipe (ref "v")

out :: T.CanonicalType -> JS.Expression () -> JS.Expression ()
out tipe x =
    case tipe of
      T.Aliased _ t -> out t x

      T.Lambda _ _
          | numArgs > 1 && numArgs < 10 ->
              func (ref ('A':show numArgs) `call` (x:values))
          | otherwise -> func (foldl (<|) x values)
          where
            ts = T.collectLambdas tipe
            numArgs = length ts - 1
            args = map (\n -> '_' : show n) [0..]
            values = zipWith inc (init ts) (map ref args)
            func body = function (take numArgs args)
                        [ JS.VarDeclStmt () [JS.VarDecl () (var "_r") (Just body)]
                        , ret (out (last ts) (ref "_r"))
                        ]

      T.Var _ -> error "type variables should not be allowed through input ports"

      T.Type (Var.Canonical Var.BuiltIn name)
          | name `elem` ["Int","Float","Bool","String"] -> x

      T.Type name
          | Var.isJson name -> V.value "Native.Json" "toJS" <| x
          | otherwise -> error "bad type got to outgoing port generation code"

      T.App f args ->
          case f : args of
            T.Type name : [t]
                | Var.isMaybe name ->
                    JS.CondExpr () (equal (JS.DotRef () x (var "ctor")) (string "Nothing"))
                                 (JS.NullLit ())
                                 (out t (JS.DotRef () x (var "_0")))

                | Var.isArray name ->
                    JS.DotRef () (obj ["_A","toJSArray"] <| x) (var "map") <| outgoing t

                | Var.isList name ->
                    JS.DotRef () (obj ["_L","toArray"] <| x) (var "map") <| outgoing t

            T.Type name : ts
                | Var.isTuple name ->
                    let convert n t = out t $ JS.DotRef () x $ var ('_':show n)
                    in  JS.ArrayLit () $ zipWith convert [0..] ts

            _ -> error "bad ADT got to outgoing port generation code"

      T.Record _ (Just _) ->
          error "bad record got to outgoing port generation code"

      T.Record fields Nothing ->
          JS.ObjectLit () keys
          where
            keys = map convert fields
            convert (f,t) = (JS.PropId () (var f), out t (JS.DotRef () x (var f)))
