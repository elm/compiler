module Generate.JavaScript.Foreign (encode, decode) where

import qualified Data.List as List
import Language.ECMAScript3.Syntax

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Type as T
import qualified AST.Variable as Var
import Generate.JavaScript.Helpers ((<|), (==>), function, ref, var)
import qualified Generate.JavaScript.Variable as Var



-- ENCODE


encode :: T.Canonical -> Expression ()
encode tipe =
  function ["v"] [ ReturnStmt () (Just (encodeHelp tipe (ref "v"))) ]


encodeHelp :: T.Canonical -> Expression () -> Expression ()
encodeHelp tipe expr =
  case tipe of
    T.Aliased _ args t ->
      encodeHelp (T.dealias args t) expr

    T.Lambda _ _ ->
      error "encodeHelp: function"

    T.Var _ ->
      error "encodeHelp: type variable"

    T.Type name
      | Var.isPrim "Float"  name -> expr
      | Var.isPrim "Int"    name -> expr
      | Var.isPrim "Bool"   name -> expr
      | Var.isPrim "String" name -> expr
      | Var.isJson name          -> expr
      | Var.isTuple name         -> NullLit ()
      | otherwise                -> error "encodeHelp: bad type"

    T.App (T.Type name) [arg]
      | Var.isMaybe name -> fromMaybe arg expr
      | Var.isList  name -> fromList arg expr
      | Var.isArray name -> fromArray arg expr
      | otherwise        -> error "encodeHelp: bad union type"

    T.App (T.Type name) args
      | Var.isTuple name -> fromTuple args expr

    T.App _ _ ->
      error "encodeHelp: bad union type"

    T.Record _ (Just _) ->
      error "encodeHelp: bad record"

    T.Record fields Nothing ->
      let
        convert (name, tipe) =
          name ==> encodeHelp tipe (DotRef () expr (var name))
      in
        ObjectLit () (map convert fields)


fromMaybe :: T.Canonical -> Expression () -> Expression ()
fromMaybe tipe expr =
  CondExpr ()
    (InfixExpr () OpStrictEq (DotRef () expr (var "ctor")) (StringLit () "Nothing"))
    (NullLit ())
    (encodeHelp tipe (DotRef () expr (var "_0")))


fromList :: T.Canonical -> Expression () -> Expression ()
fromList tipe expr =
  let
    array =
      Var.coreNative "List" "toArray" <| expr
  in
    DotRef () array (var "map") <| encode tipe


fromArray :: T.Canonical -> Expression () -> Expression ()
fromArray tipe expr =
  let
    array =
      Var.coreNative "Array" "toJSArray" <| expr
  in
    DotRef () array (var "map") <| encode tipe


fromTuple :: [T.Canonical] -> Expression () -> Expression ()
fromTuple types expr =
  let
    convert subType n =
      encodeHelp subType $ DotRef () expr (var ('_':show n))
  in
    ArrayLit () (zipWith convert types [0..])



-- SUBSCRIPTIONS


decode :: T.Canonical -> Opt.Expr
decode tipe =
  case tipe of
    T.Lambda _ _ ->
      error "functions should not be allowed through input ports"

    T.Var _ ->
      error "type variables should not be allowed through input ports"

    T.Aliased _ args t ->
      decode (T.dealias args t)

    T.Type name
      | Var.isPrim "Float"  name -> to "float"
      | Var.isPrim "Int"    name -> to "int"
      | Var.isPrim "Bool"   name -> to "bool"
      | Var.isPrim "String" name -> to "string"
      | Var.isJson name          -> to "value"
      | Var.isTuple name         -> toTuple0
      | otherwise                -> error "decode: bad type"

    T.App (T.Type name) [arg]
      | Var.isMaybe name -> toMaybe arg
      | Var.isList  name -> to "list"  <== [decode arg]
      | Var.isArray name -> to "array" <== [decode arg]
      | otherwise        -> error "decode: bad union type"

    T.App (T.Type name) args
      | Var.isTuple name -> toTuple args

    T.App _ _ ->
      error "decode: bad union type"

    T.Record _ (Just _) ->
      error "decode: bad record"

    T.Record fields Nothing ->
      toRecord fields


-- DECODE HELPERS


inJsonDecode :: String -> Var.Canonical
inJsonDecode name =
  Var.inCore ["Json","Decode"] name


to :: String -> Opt.Expr
to name =
  Opt.Var (inJsonDecode name)


field :: String -> Opt.Expr -> Opt.Expr
field name decoder =
  to "field" <== [ Opt.Literal (Literal.Str name), decoder ]


index :: Int -> Opt.Expr -> Opt.Expr
index index decoder =
  to "index" <== [ Opt.Literal (Literal.IntNum index), decoder ]


(<==) :: Opt.Expr -> [Opt.Expr] -> Opt.Expr
(<==) func args =
  Opt.Call func args



-- COMPLEX DECODE HELPERS


toMaybe :: T.Canonical -> Opt.Expr
toMaybe tipe =
  let
    maybe tag =
      Opt.Var (Var.inCore ["Maybe"] tag)
  in
    to "oneOf" <==
      [ Opt.ExplicitList
          [ to "null" <== [ maybe "Nothing" ]
          , to "map" <== [ maybe "Just", decode tipe ]
          ]
      ]


toTuple0 :: Opt.Expr
toTuple0 =
  to "null" <== [ Opt.Data "_Tuple0" [] ]


toTuple :: [T.Canonical] -> Opt.Expr
toTuple types =
  let
    size =
      length types

    pairs =
      zipWith (,) [ 0 .. size - 1] types

    toVar i =
      'x' : show i

    makeTuple =
      to "succeed" <==
        [ Opt.Data
            ("_Tuple" ++ show size)
            (map (Opt.Var . Var.local . toVar . fst) pairs)
        ]

    andThen (i, tipe) decoder =
      to "andThen" <== [ Opt.Function [toVar i] decoder, index i (decode tipe) ]
  in
    List.foldr andThen makeTuple pairs


toRecord :: [(String, T.Canonical)] -> Opt.Expr
toRecord fields =
  let
    toFieldExpr (key, _) =
      (key, Opt.Var (Var.local key))

    finalDecoder =
      to "succeed" <== [ Opt.Record (map toFieldExpr fields) ]

    andThen (key, tipe) decoder =
      to "andThen" <== [ Opt.Function [key] decoder, field key (decode tipe) ]
  in
    List.foldr andThen finalDecoder fields
