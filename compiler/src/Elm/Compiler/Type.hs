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



-- TYPES
--
-- Types that can be encoded as JSON.


data Type
  = Lambda Type Type [Type]
  | Var T.Var
  | Type T.Name [Type]
  | Unit
  | Tuple Type Type [Type]
  | Empty
  | Record (N.Name,Type) [(N.Name,Type)]
  | ExtRec (N.Name,Type) [(N.Name,Type)] T.Var


data DebugMetadata =
  DebugMetadata
    { _message :: Type
    , _aliases :: [Alias]
    , _unions :: [Union]
    }


data Alias = Alias T.Name [T.Var] Type
data Union = Union T.Name [T.Var] [(N.Name, [Type])]



-- TO DOC


toDoc :: RT.Context -> Type -> D.Doc
toDoc context tipe =
  case tipe of
    Lambda a b cs -> RT.lambda context (toDoc RT.Func a) (toDoc RT.Func b) (map (toDoc RT.Func) cs)
    Var n         -> D.fromVar n
    Type n xs     -> RT.apply context (D.fromType n) (map (toDoc RT.App) xs)
    Unit          -> "()"
    Tuple a b cs  -> RT.tuple (toDoc RT.None a) (toDoc RT.None b) (map (toDoc RT.None) cs)
    Empty         -> "{}"
    Record f fs   -> RT.record1 (go f) (map go fs)
    ExtRec f fs e -> RT.extrec1 (go f) (map go fs) (D.fromVar e)
  where
    go (f,t) = RT.Field f (toDoc RT.None t)



-- JSON for TYPE


encode :: Type -> E.Value
encode tipe =
  E.chars $ D.toLine (toDoc RT.None tipe)


decoder :: D.Decoder () Type
decoder =
  let
    parser =
      P.specialize (\_ _ -> ()) (fromSrcType . fst <$> Type.expression)
  in
  D.customString parser (\_ -> ())


fromSrcType :: Src.Type -> Type
fromSrcType (A.At _ astType) =
  case astType of
    Src.TLambda x y zs     -> Lambda (fromSrcType x) (fromSrcType y) (map fromSrcType zs)
    Src.TVar x             -> Var x
    Src.TUnit              -> Unit
    Src.TTuple a b cs      -> Tuple (fromSrcType a) (fromSrcType b) (map fromSrcType cs)
    Src.TType       _ n xs -> Type n (map fromSrcType xs)
    Src.TTypeQual _ _ n xs -> Type n (map fromSrcType xs)
    Src.TEmpty             -> Empty
    Src.TRecord f fs       -> Record (go f) (map go fs)
    Src.TExtRec f fs ext   -> ExtRec (go f) (map go fs) (A.toValue ext)
  where
    go (A.At _ f, t) = (f, fromSrcType t)



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
