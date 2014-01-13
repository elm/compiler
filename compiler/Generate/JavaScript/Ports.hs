{-# OPTIONS_GHC -W #-}
module Generate.JavaScript.Ports (incoming, outgoing) where

import Generate.JavaScript.Helpers
import qualified SourceSyntax.Helpers as Help
import SourceSyntax.Type
import Language.ECMAScript3.Syntax

check :: a -> [a -> Expression ()] -> Expression () -> Expression ()
check x checks continue =
    CondExpr () (foldl1 (InfixExpr () OpLOr) (map ($x) checks)) continue throw
  where
    throw = obj "_E.raise" <| string "invalid input to port"

incoming :: Type -> Expression ()
incoming t = ["v"] ==> go t (ref "v")
  where
    go :: Type -> Expression () -> Expression ()
    go tipe x =
     case tipe of
      Lambda _ _ -> error "functions should not be allowed through input ports"
      Var _ -> error "type variables should not be allowed through input ports"
      Data ctor []
          | ctor == "Int"    -> elm "Int"    [typeof "number"]
          | ctor == "Float"  -> elm "Float"  [typeof "number"]
          | ctor == "Bool"   -> elm "Bool"   [typeof "boolean"]
          | ctor == "String" -> elm "String" [typeof "string", instanceof "String"]
          | ctor == "JavaScript.JSNumber" -> js [typeof "number"]
          | ctor == "JavaScript.JSBool"   -> js [typeof "boolean"]
          | ctor == "JavaScript.JSString" -> js [typeof "string", instanceof "String"]
          where
            elm out checks = check x checks (obj ("_J.to" ++ out) <| x)
            js checks = check x checks x

      Data ctor [t]
          | ctor == "Maybe.Maybe" ->
              CondExpr () (equal x (NullLit ()))
                          (obj "Maybe.Nothing")
                          (obj "Maybe.Just" <| go t x)

          | ctor == "Signal.Signal" ->
              obj "Native.Ports.incomingSignal" <| incoming t

          | ctor == "_List" ->
              check x [instanceof "Array"] (obj "_J.toList" <| array)
              where
                array = DotRef () x (var "map") <| incoming t

      Data ctor ts | Help.isTuple ctor -> check x [instanceof "Array"] tuple
          where
            tuple = ObjectLit () $ (PropId () (var "ctor"), string ctor) : values
            values = zipWith convert [0..] ts
            convert n t = ( PropId () $ var ('_':show n)
                          , go t (BracketRef () x (IntLit () n)))

      Data _ _ -> error "bad ADT got to port generation code"

      Record _ (Just _) -> error "bad record got to port generation code"

      Record fields Nothing ->
          check x (typeof "object" : map (member . fst) fields) object
          where
            object = ObjectLit () $ (PropId () (var "_"), ObjectLit () []) : keys
            keys = map convert fields
            convert (f,t) = (PropId () (var f), go t (DotRef () x (var f)))

outgoing t = ["v"] ==> go t (ref "v")
  where
    go :: Type -> Expression () -> Expression ()
    go tipe x =
     case tipe of
      Lambda _ _ -> error "unimplemented"
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

          | ctor == "Signal.Signal" ->
              obj "Native.Ports.outgoingSignal" <| incoming t

          | ctor == "_List" ->
              DotRef () (obj "_J.fromList" <| x) (var "map") <| incoming t

      Data ctor ts | Help.isTuple ctor ->
          ArrayLit () $ zipWith convert [0..] ts
          where
            convert n t = go t $ DotRef () x $ var ('_':show n)

      Data _ _ -> error "bad ADT got to port generation code"

      Record _ (Just _) -> error "bad record got to port generation code"

      Record fields Nothing ->
          ObjectLit () keys
          where
            keys = map convert fields
            convert (f,t) = (PropId () (var f), go t (DotRef () x (var f)))
