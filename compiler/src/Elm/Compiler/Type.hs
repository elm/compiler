{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
  ( Type(..)
  , RT.Context(..)
  , toDoc
  , DebugMetadata(..)
  , Alias(..)
  , Union(..)
  , encode
  , decoder
  , encodeMetadata
  )
  where


import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified AST.Source as Src
import qualified Parse.Primitives as P
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L

import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))



-- TYPES


data Type
  = Lambda Type Type
  | Var Name.Name
  | Type Name.Name [Type]
  | Record [(Name.Name, Type)] (Maybe Name.Name)
  | Unit
  | Tuple Type Type [Type]


data DebugMetadata =
  DebugMetadata
    { _message :: Type
    , _aliases :: [Alias]
    , _unions :: [Union]
    }


data Alias = Alias Name.Name [Name.Name] Type
data Union = Union Name.Name [Name.Name] [(Name.Name, [Type])]



-- TO DOC


toDoc :: L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
  case tipe of
    Lambda _ _ ->
      let
        a:b:cs =
          map (toDoc localizer RT.Func) (collectLambdas tipe)
      in
      RT.lambda context a b cs

    Var name ->
      D.fromName name

    Unit ->
      "()"

    Tuple a b cs ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (map (toDoc localizer RT.None) cs)

    Type name args ->
      RT.apply
        context
        (D.fromName name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record
        (map (entryToDoc localizer) fields)
        (fmap D.fromName ext)


entryToDoc :: L.Localizer -> (Name.Name, Type) -> (D.Doc, D.Doc)
entryToDoc localizer (field, fieldType) =
  ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body ->
      arg : collectLambdas body

    _ ->
      [tipe]



-- JSON for TYPE


encode :: Type -> E.Value
encode tipe =
  E.string $ Utf8.fromChars $ D.toLine (toDoc L.empty RT.None tipe)


decoder :: D.Decoder () Type
decoder =
  do  str <- D.string
      case P.fromByteString Type.expression (error "TODO turn Utf8 to something that can be parsed" str) of
        P.Ok (tipe, _, _) _ ->
          return (fromRawType tipe)

        P.Err _ _ _ _ ->
          D.failure ()


fromRawType :: Src.Type -> Type
fromRawType (A.At _ astType) =
  case astType of
    Src.TLambda t1 t2 ->
        Lambda (fromRawType t1) (fromRawType t2)

    Src.TVar x ->
        Var x

    Src.TUnit ->
        Unit

    Src.TTuple a b cs ->
        Tuple
          (fromRawType a)
          (fromRawType b)
          (map fromRawType cs)

    Src.TType _ name args ->
        Type name (map fromRawType args)

    Src.TTypeQual _ _ name args ->
        Type name (map fromRawType args)

    Src.TRecord fields ext ->
        let fromField (A.At _ field, tipe) = (field, fromRawType tipe) in
        Record
          (map fromField fields)
          (fmap A.toValue ext)



-- JSON for PROGRAM


encodeMetadata :: DebugMetadata -> E.Value
encodeMetadata (DebugMetadata msg aliases unions) =
  E.object
    [ "message" ==> encode msg
    , "aliases" ==> E.object (map toAliasField aliases)
    , "unions" ==> E.object (map toUnionField unions)
    ]


toAliasField :: Alias -> ( Utf8.String, E.Value )
toAliasField (Alias name args tipe) =
  Name.toUtf8 name ==>
    E.object
      [ "args" ==> E.list E.name args
      , "type" ==> encode tipe
      ]


toUnionField :: Union -> ( Utf8.String, E.Value )
toUnionField (Union name args constructors) =
  Name.toUtf8 name ==>
    E.object
      [ "args" ==> E.list E.name args
      , "tags" ==> E.object (map toCtorObject constructors)
      ]


toCtorObject :: (Name.Name, [Type]) -> ( Utf8.String, E.Value )
toCtorObject (name, args) =
  Name.toUtf8 name ==> E.list encode args
