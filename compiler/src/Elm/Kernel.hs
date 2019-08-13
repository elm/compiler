{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, OverloadedStrings, UnboxedTuples #-}
module Elm.Kernel
  ( Content(..)
  , Chunk(..)
  , fromByteString
  , countFields
  )
  where


import Control.Monad (liftM, liftM2)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.ByteString.Internal as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified AST.Source as Src
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Parse.Module as Module
import qualified Parse.Space as Space
import qualified Parse.Variable as Var
import Parse.Primitives hiding (fromByteString)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- CHUNK


data Chunk
  = JS B.ByteString
  | ElmVar ModuleName.Canonical Name.Name
  | JsVar Name.Name Name.Name
  | ElmField Name.Name
  | JsField Int
  | JsEnum Int
  | Debug
  | Prod



-- COUNT FIELDS


countFields :: [Chunk] -> Map.Map Name.Name Int
countFields chunks =
  foldr addField Map.empty chunks


addField :: Chunk -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addField chunk fields =
  case chunk of
    JS _       -> fields
    ElmVar _ _ -> fields
    JsVar _ _  -> fields
    ElmField f -> Map.insertWith (+) f 1 fields
    JsField _  -> fields
    JsEnum _   -> fields
    Debug      -> fields
    Prod       -> fields



-- FROM FILE


data Content =
  Content [Src.Import] [Chunk]


type Foreigns =
  Map.Map ModuleName.Raw Pkg.Name


fromByteString :: Pkg.Name -> Foreigns -> B.ByteString -> Maybe Content
fromByteString pkg foreigns bytes =
  case P.fromByteString (parser pkg foreigns) toError bytes of
    Right content ->
      Just content

    Left () ->
      Nothing


parser :: Pkg.Name -> Foreigns -> Parser () Content
parser pkg foreigns =
  do  word2 0x2F 0x2A {-/*-} toError
      Space.chomp ignoreError
      Space.checkFreshLine toError
      imports <- specialize ignoreError (Module.chompImports [])
      word2 0x2A 0x2F {-*/-} toError
      chunks <- parseChunks (toVarTable pkg foreigns imports) Map.empty Map.empty
      return (Content imports chunks)


toError :: Row -> Col -> ()
toError _ _ =
  ()


ignoreError :: a -> Row -> Col -> ()
ignoreError _ _ _ =
  ()



-- PARSE CHUNKS


parseChunks :: VarTable -> Enums -> Fields -> Parser () [Chunk]
parseChunks vtable enums fields =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr _ ->
    let
      (# chunks, newPos, newRow, newCol #) =
        chompChunks vtable enums fields src pos end row col pos []
    in
    if newPos == end then
      cok chunks (P.State src newPos end indent newRow newCol)
    else
      cerr row col toError


chompChunks :: VarTable -> Enums -> Fields -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Row -> Col -> Ptr Word8 -> [Chunk] -> (# [Chunk], Ptr Word8, Row, Col #)
chompChunks vs es fs src pos end row col lastPos revChunks =
  if pos >= end then
    let !js = toByteString src lastPos end in
    (# reverse (JS js : revChunks), pos, row, col #)

  else
    let !word = unsafeIndex pos in
    if word == 0x5F {-_-} then
      let
        !pos1 = plusPtr pos 1
        !pos3 = plusPtr pos 3
      in
      if pos3 <= end && unsafeIndex pos1 == 0x5F {-_-} then
        let !js = toByteString src lastPos pos in
        chompTag vs es fs src pos3 end row (col + 3) (JS js : revChunks)
      else
        chompChunks vs es fs src pos1 end row (col + 1) lastPos revChunks

    else if word == 0x0A {-\n-} then
      chompChunks vs es fs src (plusPtr pos 1) end (row + 1) 1 lastPos revChunks

    else
      let
        !newPos = plusPtr pos (getCharWidth word)
      in
      chompChunks vs es fs src newPos end row (col + 1) lastPos revChunks


toByteString :: ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> B.ByteString
toByteString src pos end =
  let
    !off = minusPtr pos (unsafeForeignPtrToPtr src)
    !len = minusPtr end pos
  in
  B.PS src off len



-- relies on external checks in chompChunks
chompTag :: VarTable -> Enums -> Fields -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Row -> Col -> [Chunk] -> (# [Chunk], Ptr Word8, Row, Col #)
chompTag vs es fs src pos end row col revChunks =
  let
    (# newPos, newCol #) = Var.chompInnerChars pos end col
    !tagPos = plusPtr pos (-1)
    !word = unsafeIndex tagPos
  in
  if word == 0x24 {-$-} then
    let
      !name = Name.fromPtr pos newPos
    in
    chompChunks vs es fs src newPos end row newCol newPos $
      ElmField name : revChunks
  else
    let
      !name = Name.fromPtr tagPos newPos
    in
    if 0x30 {-0-} <= word && word <= 0x39 {-9-} then
      let
        (enum, newEnums) =
          lookupEnum (word - 0x30) name es
      in
      chompChunks vs newEnums fs src newPos end row newCol newPos $
        JsEnum enum : revChunks

    else if 0x61 {-a-} <= word && word <= 0x7A {-z-} then
      let
        (field, newFields) =
          lookupField name fs
      in
      chompChunks vs es newFields src newPos end row newCol newPos $
        JsField field : revChunks

    else if name == "DEBUG" then
      chompChunks vs es fs src newPos end row newCol newPos (Debug : revChunks)

    else if name == "PROD" then
      chompChunks vs es fs src newPos end row newCol newPos (Prod : revChunks)

    else
      case Map.lookup name vs of
        Just chunk ->
          chompChunks vs es fs src newPos end row newCol newPos (chunk : revChunks)

        Nothing ->
          (# revChunks, pos, row, col #)



-- FIELDS


type Fields =
  Map.Map Name.Name Int


lookupField :: Name.Name -> Fields -> (Int, Fields)
lookupField name fields =
  case Map.lookup name fields of
    Just n ->
      ( n, fields )

    Nothing ->
      let n = Map.size fields in
      ( n, Map.insert name n fields )



-- ENUMS


type Enums =
  Map.Map Word8 (Map.Map Name.Name Int)


lookupEnum :: Word8 -> Name.Name -> Enums -> (Int, Enums)
lookupEnum word var allEnums =
  let
    enums =
      Map.findWithDefault Map.empty word allEnums
  in
    case Map.lookup var enums of
      Just n ->
        ( n, allEnums )

      Nothing ->
        let n = Map.size enums in
        ( n, Map.insert word (Map.insert var n enums) allEnums )



-- PROCESS IMPORTS


type VarTable =
  Map.Map Name.Name Chunk


toVarTable :: Pkg.Name -> Foreigns -> [Src.Import] -> VarTable
toVarTable pkg foreigns imports =
  List.foldl' (addImport pkg foreigns) Map.empty imports


addImport :: Pkg.Name -> Foreigns -> VarTable -> Src.Import -> VarTable
addImport pkg foreigns vtable (Src.Import (A.At _ importName) maybeAlias exposing) =
  if Name.isKernel importName then
    case maybeAlias of
      Just _ ->
        error ("cannot use `as` with kernel import of: " ++ Name.toChars importName)

      Nothing ->
        let
          home = Name.getKernel importName
          add table name =
            Map.insert (Name.sepBy 0x5F {-_-} home name) (JsVar home name) table
        in
        List.foldl' add vtable (toNames exposing)

  else
    let
      home = ModuleName.Canonical (Map.findWithDefault pkg importName foreigns) importName
      prefix = toPrefix importName maybeAlias
      add table name =
        Map.insert (Name.sepBy 0x5F {-_-} prefix name) (ElmVar home name) table
    in
    List.foldl' add vtable (toNames exposing)


toPrefix :: Name.Name -> Maybe Name.Name -> Name.Name
toPrefix home maybeAlias =
  case maybeAlias of
    Just alias ->
      alias

    Nothing ->
      if Name.hasDot home then
        error ("kernel imports with dots need an alias: " ++ show (Name.toChars home))
      else
        home


toNames :: Src.Exposing -> [Name.Name]
toNames exposing =
  case exposing of
    Src.Open ->
      error "cannot have `exposing (..)` in kernel code."

    Src.Explicit exposedList ->
      map toName exposedList


toName :: Src.Exposed -> Name.Name
toName exposed =
  case exposed of
    Src.Lower (A.At _ name) ->
      name

    Src.Upper (A.At _ name) Src.Private ->
      name

    Src.Upper _ (Src.Public _) ->
      error "cannot have Maybe(..) syntax in kernel code header"

    Src.Operator _ _ ->
      error "cannot use binops in kernel code"



-- BINARY


instance Binary Chunk where
  put chunk =
    case chunk of
      JS a       -> putWord8 0 >> put a
      ElmVar a b -> putWord8 1 >> put a >> put b
      JsVar a b  -> putWord8 2 >> put a >> put b
      ElmField a -> putWord8 3 >> put a
      JsField a  -> putWord8 4 >> put a
      JsEnum a   -> putWord8 5 >> put a
      Debug      -> putWord8 6
      Prod       -> putWord8 7

  get =
    do  word <- getWord8
        case word of
          0 -> liftM  JS get
          1 -> liftM2 ElmVar get get
          2 -> liftM2 JsVar get get
          3 -> liftM  ElmField get
          4 -> liftM  JsField get
          5 -> liftM  JsEnum get
          6 -> return Debug
          7 -> return Prod
          _ -> error "problem deserializing Elm.Kernel.Chunk"
