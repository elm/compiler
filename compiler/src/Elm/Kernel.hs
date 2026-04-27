{-# LANGUAGE BangPatterns, EmptyDataDecls, ExtendedLiterals, MagicHash,
OverloadedStrings, UnboxedTuples
#-}
module Elm.Kernel
  ( Content(..)
  , Chunk(..)
  , fromByteString
  , countFields
  )
  where


import Control.Monad (liftM, liftM2)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.ByteString.Internal as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.Word (Word8(..))

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
  = JS BS.ByteString
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


fromByteString :: Pkg.Name -> Foreigns -> BS.ByteString -> IO (Maybe Content)
fromByteString pkg foreigns bytes =
  do  result <- P.fromByteString (parser pkg foreigns) toError bytes
      case result of
        Right content -> pure $ Just content
        Left  ()      -> pure $ Nothing


parser :: Pkg.Name -> Foreigns -> Parser () Content
parser pkg foreigns =
  do  word2 0x2F#Word8 0x2A#Word8 {-/*-} toError
      Space.chomp ignoreError
      Space.checkFreshLine toError
      imports <- specialize ignoreError (Module.chompImports [])
      word2 0x2A#Word8 0x2F#Word8 {-*/-} toError
      chunks <- parseChunks (toVarTable pkg foreigns imports) Map.empty Map.empty
      return (Content imports chunks)


toError :: Cursor -> ()
toError _ =
  ()


ignoreError :: a -> Cursor -> ()
ignoreError _ _ =
  ()



-- PARSE CHUNKS


parseChunks :: VarTable -> Enums -> Fields -> Parser () [Chunk]
parseChunks vtable enums fields =
  P.Parser $ \fpc (P.State pos end indent cur) cok _ cerr _ ->
    do  !(Answer chunks newPos newCur) <- chompChunks vtable enums fields fpc pos end cur pos []
        if eqAddr newPos end
          then cok chunks (P.State newPos end indent newCur)
          else cerr cur toError


data Answer = Answer [Chunk] Addr# Cursor


chompChunks :: VarTable -> Enums -> Fields -> ForeignPtrContents -> Addr# -> Addr# -> Cursor -> Addr# -> [Chunk] -> IO Answer
chompChunks vs es fs fpc pos end cur lastPos revChunks =
  if notLtAddr pos end then
    let !js = toBS fpc lastPos end in
    pure $ Answer (reverse (JS js : revChunks)) pos cur

  else
    case indexWord8OffAddr# pos 0# of
      0x5F#Word8 {-_-} ->
        let
          !pos1 = plusAddr# pos 1#
          !pos3 = plusAddr# pos 3#
        in
        if leAddr pos3 end && eqIndex pos1 0# 0x5F#Word8 {-_-} then
          let !js = toBS fpc lastPos pos in
          chompTag vs es fs fpc pos3 end (slide cur 3#Word64) (JS js : revChunks)
        else
          chompChunks vs es fs fpc pos1 end (slide cur 1#Word64) lastPos revChunks

      0x0A#Word8 {-\n-} ->
        chompChunks vs es fs fpc (plusAddr# pos 1#) end (newline cur) lastPos revChunks

      word ->
        do  let !newPos = skipUtf8 pos end word
            if eqAddr pos newPos
              then error "kernel must be UTF8"
              else chompChunks vs es fs fpc newPos end (slide cur 1#Word64) lastPos revChunks


toBS :: ForeignPtrContents -> Addr# -> Addr# -> BS.ByteString
toBS fpc pos end =
  BS.BS (ForeignPtr pos fpc) (I# (minusAddr# end pos))



-- relies on external checks in chompChunks
chompTag :: VarTable -> Enums -> Fields -> ForeignPtrContents -> Addr# -> Addr# -> Cursor -> [Chunk] -> IO Answer
chompTag vs es fs fpc pos end cur revChunks =
  let
    !(# newPos, newCur #) = Var.chompInnerChars pos end cur
    !tagPos = plusAddr# pos (-1#)
    !word = W8# (indexWord8OffAddr# tagPos 0#)
  in
  if word == 0x24 {-$-} then
    do  name <- Name.fromAddr pos newPos
        chompChunks vs es fs fpc newPos end newCur newPos $
          ElmField name : revChunks
  else
    do  name <- Name.fromAddr tagPos newPos
        case () of
          _ | 0x30 {-0-} <= word && word <= 0x39 {-9-} ->
                do  let (enum, newEnums) = lookupEnum (word - 0x30) name es
                    chompChunks vs newEnums fs fpc newPos end newCur newPos $
                      JsEnum enum : revChunks

            | 0x61 {-a-} <= word && word <= 0x7A {-z-} ->
                do  let (field, newFields) = lookupField name fs
                    chompChunks vs es newFields fpc newPos end newCur newPos $
                      JsField field : revChunks

            | name == "DEBUG" -> chompChunks vs es fs fpc newPos end newCur newPos (Debug : revChunks)
            | name == "PROD"  -> chompChunks vs es fs fpc newPos end newCur newPos (Prod : revChunks)
            | otherwise ->
                case Map.lookup name vs of
                  Just chunk -> chompChunks vs es fs fpc newPos end newCur newPos (chunk : revChunks)
                  Nothing    -> pure $ Answer (reverse revChunks) pos cur



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
