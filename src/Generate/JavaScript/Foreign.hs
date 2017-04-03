{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Foreign (encode, decode) where

import Prelude hiding (maybe)
import Control.Monad (zipWithM)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Helpers as JS (function, ref)
import Generate.JavaScript.Helpers ((<|), (==>))
import qualified Generate.JavaScript.Variable as JS (Generator, global)



-- ENCODE


encode :: T.Canonical -> JS.Generator JS.Expr
encode tipe =
  do  js <- encodeHelp tipe (JS.ref "v")
      return $ JS.function ["v"] [ JS.Return (Just js) ]


encodeHelp :: T.Canonical -> JS.Expr -> JS.Generator JS.Expr
encodeHelp tipe expr =
  case tipe of
    T.Aliased _ args t ->
      encodeHelp (T.dealias args t) expr

    T.Lambda _ _ ->
      error "encodeHelp: function"

    T.Var _ ->
      error "encodeHelp: type variable"

    T.Type name
      | Var.isPrim "Float"  name -> pure expr
      | Var.isPrim "Int"    name -> pure expr
      | Var.isPrim "Bool"   name -> pure expr
      | Var.isPrim "String" name -> pure expr
      | Var.isJson name          -> pure expr
      | Var.isTuple name         -> pure JS.Null
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
        convert (k, t) =
          (==>) k <$> encodeHelp t (JS.DotRef expr (JS.Id k))
      in
        JS.Object <$> traverse convert fields


fromMaybe :: T.Canonical -> JS.Expr -> JS.Generator JS.Expr
fromMaybe tipe expr =
  do  js <- encodeHelp tipe (JS.DotRef expr (JS.Id "_0"))
      let isNothing = JS.Infix JS.OpStrictEq (JS.DotRef expr (JS.Id "ctor")) (JS.String "Nothing")
      return $ JS.If isNothing JS.Null js


fromList :: T.Canonical -> JS.Expr -> JS.Generator JS.Expr
fromList tipe expr =
  do  let var = Var.Global (ModuleName.inCore "Elm.Kernel.List") "toArray"
      toArray <- JS.global var
      encoder <- encode tipe
      return $ JS.DotRef (toArray <| expr) (JS.Id "map") <| encoder


fromArray :: T.Canonical -> JS.Expr -> JS.Generator JS.Expr
fromArray tipe expr =
  do  let var = Var.Global (ModuleName.inCore "Elm.Kernel.Array") "toJSArray"
      toArray <- JS.global var
      encoder <- encode tipe
      return $ JS.DotRef (toArray <| expr) (JS.Id "map") <| encoder


fromTuple :: [T.Canonical] -> JS.Expr -> JS.Generator JS.Expr
fromTuple types expr =
  let
    convert subType n =
      encodeHelp subType $ JS.DotRef expr (JS.Id (Text.pack ('_':show n)))
  in
    JS.Array <$> zipWithM convert types [ 0 .. length types ]



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


to :: Text -> Opt.Expr
to name =
  Opt.VarGlobal (Var.Global jsonDecode name)


jsonDecode :: ModuleName.Canonical
jsonDecode =
  ModuleName.inCore "Json.Decode"


field :: Text -> Opt.Expr -> Opt.Expr
field name decoder =
  to "field" <== [ Opt.Literal (Literal.Str name), decoder ]


index :: Int -> Opt.Expr -> Opt.Expr
index i decoder =
  to "index" <== [ Opt.Literal (Literal.IntNum i), decoder ]


(<==) :: Opt.Expr -> [Opt.Expr] -> Opt.Expr
(<==) func args =
  Opt.Call func args



-- COMPLEX DECODE HELPERS


toMaybe :: T.Canonical -> Opt.Expr
toMaybe tipe =
  let
    maybe tag =
      Opt.VarGlobal (Var.Global (ModuleName.inCore "Maybe") tag)
  in
    to "oneOf" <==
      [ Opt.List
          [ to "null" <== [ maybe "Nothing" ]
          , to "map" <== [ maybe "Just", decode tipe ]
          ]
      ]


toTuple0 :: Opt.Expr
toTuple0 =
  to "null" <== [ Opt.Ctor "_Tuple0" [] ]


toTuple :: [T.Canonical] -> Opt.Expr
toTuple types =
  let
    size =
      length types

    pairs =
      zipWith (,) [ 0 .. size - 1] types

    toVar i =
      Text.pack ('x' : show i)

    makeTuple =
      to "succeed" <==
        [ Opt.Ctor
            (Text.pack ("_Tuple" ++ show size))
            (map (Opt.VarLocal . toVar . fst) pairs)
        ]

    andThen (i, tipe) decoder =
      to "andThen" <== [ Opt.Function [toVar i] decoder, index i (decode tipe) ]
  in
    List.foldr andThen makeTuple pairs


toRecord :: [(Text, T.Canonical)] -> Opt.Expr
toRecord fields =
  let
    toFieldExpr (key, _) =
      (key, Opt.VarLocal key)

    finalDecoder =
      to "succeed" <== [ Opt.Record (map toFieldExpr fields) ]

    andThen (key, tipe) decoder =
      to "andThen" <== [ Opt.Function [key] decoder, field key (decode tipe) ]
  in
    List.foldr andThen finalDecoder fields
