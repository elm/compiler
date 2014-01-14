{-# OPTIONS_GHC -W #-}
module Generate.JavaScript.Ports (incoming, outgoing) where

import Generate.JavaScript.Helpers
import qualified SourceSyntax.Helpers as Help
import SourceSyntax.Type as T
import Language.ECMAScript3.Syntax

data JSType = JSNumber | JSBoolean | JSString | JSArray | JSObject [String]
    deriving Show

check :: Expression () -> JSType -> Expression () -> Expression ()
check x jsType continue =
    CondExpr () (jsFold OpLOr checks x) continue throw
  where
    jsFold op checks value = foldl1 (InfixExpr () op) (map ($value) checks)
    throw = obj "_E.raise" <| InfixExpr () OpAdd msg x
    msg = string ("invalid input, expecting " ++ show jsType ++ " but got ")
    checks = case jsType of
               JSNumber  -> [typeof "number"]
               JSBoolean -> [typeof "boolean"]
               JSString  -> [typeof "string", instanceof "String"]
               JSArray   -> [instanceof "Array"]
               JSObject fields -> [jsFold OpLAnd (typeof "object" : map member fields)]

incoming :: Type -> Expression ()
incoming tipe =
  case tipe of
    Data "Signal.Signal" [t] ->
        obj "Native.Ports.incomingSignal" <| incoming t
    _ -> ["v"] ==> inc tipe (ref "v")

inc :: Type -> Expression () -> Expression ()
inc tipe x =
     case tipe of
      Lambda _ _ -> error "functions should not be allowed through input ports"
      Var _ -> error "type variables should not be allowed through input ports"
      Data ctor []
          | ctor == "Int"    -> elm JSNumber
          | ctor == "Float"  -> elm JSNumber
          | ctor == "Bool"   -> elm JSBoolean
          | ctor == "String" -> elm JSString
          | ctor == "JavaScript.JSNumber" -> js JSNumber
          | ctor == "JavaScript.JSBool"   -> js JSBoolean
          | ctor == "JavaScript.JSString" -> js JSString
          where
            elm checks = check x checks (obj ("_J.to" ++ ctor) <| x)
            js checks = check x checks x

      Data ctor [t]
          | ctor == "Maybe.Maybe" ->
              CondExpr () (equal x (NullLit ()))
                          (obj "Maybe.Nothing")
                          (obj "Maybe.Just" <| inc t x)

          | ctor == "_List" ->
              check x JSArray (obj "_J.toList" <| array)
              where
                array = DotRef () x (var "map") <| incoming t

      Data ctor ts | Help.isTuple ctor -> check x JSArray tuple
          where
            tuple = ObjectLit () $ (PropId () (var "ctor"), string ctor) : values
            values = zipWith convert [0..] ts
            convert n t = ( PropId () $ var ('_':show n)
                          , inc t (BracketRef () x (IntLit () n)))

      Data _ _ -> error "bad ADT got to port generation code"

      Record _ (Just _) -> error "bad record got to port generation code"

      Record fields Nothing -> check x (JSObject (map fst fields)) object
          where
            object = ObjectLit () $ (PropId () (var "_"), ObjectLit () []) : keys
            keys = map convert fields
            convert (f,t) = (PropId () (var f), inc t (DotRef () x (var f)))

outgoing tipe =
  case tipe of
    Data "Signal.Signal" [t] ->
        obj "Native.Ports.outgoingSignal" <| outgoing t
    _ -> ["v"] ==> out tipe (ref "v")

out :: Type -> Expression () -> Expression ()
out tipe x =
    case tipe of
      Lambda _ _
          | numArgs > 1 && numArgs < 10 ->
              func (ref ('A':show numArgs) `call` (x:values))
          | otherwise -> func (foldl (<|) x values)
          where
            ts = T.collectLambdas tipe
            numArgs = length ts - 1
            args = map (\n -> '_' : show n) [0..]
            values = zipWith inc (init ts) (map ref args)
            func body = function (take numArgs args)
                        [ VarDeclStmt () [VarDecl () (var "_r") (Just body)]
                        , ret (out (last ts) (ref "_r"))
                        ]

      Var _ -> error "type variables should not be allowed through input ports"
      Data ctor []
          | ctor `elem` ["Int","Float","Bool","String"] -> obj ("_J.from" ++ ctor) <| x
          | ctor `elem` jsPrims -> x
          where
            jsPrims = map ("JavaScript.JS"++) ["Number","Bool","String"]

      Data ctor [t]
          | ctor == "Maybe.Maybe" ->
              CondExpr () (equal (DotRef () x (var "ctor")) (string "Nothing"))
                          (NullLit ())
                          (DotRef () x (var "_0"))

          | ctor == "_List" ->
              DotRef () (obj "_J.fromList" <| x) (var "map") <| outgoing t

      Data ctor ts | Help.isTuple ctor ->
          ArrayLit () $ zipWith convert [0..] ts
          where
            convert n t = out t $ DotRef () x $ var ('_':show n)

      Data _ _ -> error "bad ADT got to port generation code"

      Record _ (Just _) -> error "bad record got to port generation code"

      Record fields Nothing ->
          ObjectLit () keys
          where
            keys = map convert fields
            convert (f,t) = (PropId () (var f), out t (DotRef () x (var f)))
