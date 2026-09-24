{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
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


import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Json.String as Json
import qualified Parse.Primitives as P
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- TYPES
--
-- Types that can be encoded as JSON.


data Type
  = Lambda Type Type
  | Var T.Var
  | Type T.Name [Type]
  | Record [(N.Name, Type)] (Maybe T.Var)
  | Unit
  | Tuple Type Type [Type]


data DebugMetadata =
  DebugMetadata
    { _message :: Type
    , _aliases :: [Alias]
    , _unions :: [Union]
    }


data Alias = Alias T.Name [T.Var] Type
data Union = Union T.Name [T.Var] [(N.Name, [Type])]



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
      D.fromVar name

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
        (D.fromType name)
        (map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record
        (map (entryToDoc localizer) fields)
        (fmap D.fromVar ext)


entryToDoc :: L.Localizer -> (N.Name, Type) -> (D.Doc, D.Doc)
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
  E.chars $ D.toLine (toDoc L.empty RT.None tipe)


decoder :: D.Decoder () Type
decoder =
  let
    parser =
      P.specialize (\_ _ -> ()) (fromRawType . fst <$> Type.expression)
  in
  D.customString parser (\_ -> ())


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
    , "aliases" ==> E.object (map toTypeAliasField aliases)
    , "unions" ==> E.object (map toCustomTypeField unions)
    ]


toTypeAliasField :: Alias -> ( Json.String, E.Value )
toTypeAliasField (Alias name args tipe) =
  ( Json.fromStringUnsafe (T.nameToString name)
  , E.object
      [ "args" ==> E.list (E.string . T.varToString) args
      , "type" ==> encode tipe
      ]
  )


toCustomTypeField :: Union -> ( Json.String, E.Value )
toCustomTypeField (Union name args constructors) =
  ( Json.fromStringUnsafe (T.nameToString name)
  , E.object
      [ "args" ==> E.list (E.string . T.varToString) args
      , "tags" ==> E.object (map toVariantObject constructors)
      ]
  )


toVariantObject :: (N.Name, [Type]) -> ( Json.String, E.Value )
toVariantObject (name, args) =
  ( Json.fromStringUnsafe (N.toString name), E.list encode args )
