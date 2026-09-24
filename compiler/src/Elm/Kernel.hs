{-# LANGUAGE BangPatterns, EmptyDataDecls, ExtendedLiterals, MagicHash,
OverloadedStrings, QuasiQuotes, TemplateHaskell, UnboxedTuples
#-}
module Elm.Kernel
  ( Content(..)
  , Chunk(..)
  , fromByteString
  , countFields
  --
  , eChunk, dChunk
  )
  where


import Control.Monad (liftM, liftM2)
import qualified Data.ByteString.Internal as BS
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Exts (isTrue#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.Word (Word8(..))

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash
import qualified String as S

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Parse.Module as Module (chompImports)
import qualified Parse.Space as Space
import qualified Parse.Variable as Var
import Parse.Primitives hiding (fromByteString)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- CHUNK


data Chunk
  = JS BS.ByteString
  | ElmVar ModuleName.Canonical N.Name
  | JsVar Module.Kernel N.Name
  | ElmField N.Name
  | JsField Int
  | JsEnum Int
  | Debug
  | Prod



-- COUNT FIELDS


countFields :: [Chunk] -> Map.Map N.Name Int
countFields chunks =
  foldr addField Map.empty chunks


addField :: Chunk -> Map.Map N.Name Int -> Map.Map N.Name Int
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
  Map.Map Module.Name Pkg.Name


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
              then $(Crash.crashIO 'chompChunks) "kernel must be UTF8"
              else chompChunks vs es fs fpc newPos end (slide cur 1#Word64) lastPos revChunks


toBS :: ForeignPtrContents -> Addr# -> Addr# -> BS.ByteString
toBS fpc pos end =
  BS.BS (ForeignPtr pos fpc) (I# (minusAddr# end pos))



-- relies on external checks in chompChunks
chompTag :: VarTable -> Enums -> Fields -> ForeignPtrContents -> Addr# -> Addr# -> Cursor -> [Chunk] -> IO Answer
chompTag vs es fs fpc pos end cur revChunks =
  let
    !(# newPos, newCur #) = Var.chompInners pos end cur
    !tagPos = plusAddr# pos (-1#)
    !word = W8# (indexWord8OffAddr# tagPos 0#)
  in
  if word == 0x24 {-$-} then
    do  name <- N.fromAddr pos newPos
        chompChunks vs es fs fpc newPos end newCur newPos $
          ElmField name : revChunks
  else
    do  name <- N.fromAddr tagPos newPos
        case () of
          _ | 0x30 {-0-} <= word && word <= 0x39 {-9-} ->
                do  let (enum, newEnums) = lookupEnum (word - 0x30) name es
                    chompChunks vs newEnums fs fpc newPos end newCur newPos $
                      JsEnum enum : revChunks

            | 0x61 {-a-} <= word && word <= 0x7A {-z-} ->
                do  let (field, newFields) = lookupField name fs
                    chompChunks vs es newFields fpc newPos end newCur newPos $
                      JsField field : revChunks

            | name == _DEBUG -> chompChunks vs es fs fpc newPos end newCur newPos (Debug : revChunks)
            | name == _PROD  -> chompChunks vs es fs fpc newPos end newCur newPos (Prod : revChunks)
            | otherwise ->
                case Map.lookup (coerce name) vs of
                  Just chunk -> chompChunks vs es fs fpc newPos end newCur newPos (chunk : revChunks)
                  Nothing    -> pure $ Answer (reverse revChunks) pos cur


{-# NOINLINE _DEBUG #-}; _DEBUG :: N.Name; _DEBUG = [N.ascii|DEBUG|]
{-# NOINLINE _PROD  #-}; _PROD  :: N.Name; _PROD  = [N.ascii|PROD|]



-- FIELDS


type Fields =
  Map.Map N.Name Int


lookupField :: N.Name -> Fields -> (Int, Fields)
lookupField name fields =
  case Map.lookup name fields of
    Just n ->
      ( n, fields )

    Nothing ->
      let n = Map.size fields in
      ( n, Map.insert name n fields )



-- ENUMS


type Enums =
  Map.Map Word8 (Map.Map N.Name Int)


lookupEnum :: Word8 -> N.Name -> Enums -> (Int, Enums)
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
  Map.Map Key Chunk


newtype Key = Key N.Name
  deriving (Eq, Ord)


toVarTable :: Pkg.Name -> Foreigns -> [Src.Import] -> VarTable
toVarTable pkg foreigns imports =
  List.foldl' (addImport pkg foreigns) Map.empty imports


addImport :: Pkg.Name -> Foreigns -> VarTable -> Src.Import -> VarTable
addImport pkg foreigns vtable (Src.Import (A.At _ importName) maybeAlias exposing) =
  if Module.isKernel importName then
    case maybeAlias of
      Just _ ->
        $(Crash.crash 'addImport) ("cannot use `as` with kernel import of: " ++ Module.toChars importName)

      Nothing ->
        let
          home = Module.getKernel importName
          add table name =
            Map.insert (toKernelKey home name) (JsVar home name) table
        in
        List.foldl' add vtable (toNames exposing)

  else
    let
      home = ModuleName.Canonical (Map.findWithDefault pkg importName foreigns) importName
      prefix = toPrefix importName maybeAlias
      add table name =
        Map.insert (toPrefixKey prefix name) (ElmVar home name) table
    in
    List.foldl' add vtable (toNames exposing)


toPrefix :: Module.Name -> Maybe Module.Prefix -> Module.Prefix
toPrefix home maybeAlias =
  case maybeAlias of
    Just alias ->
      alias

    Nothing ->
      if hasDot home
      then $(Crash.crash 'toPrefix) ("kernel imports with dots need an alias: " ++ show (Module.toChars home))
      else Module.toPrefix home


toKernelKey :: Module.Kernel -> N.Name -> Key
toKernelKey prefix name =
  Key $ N.fromString $
    S.join (Module.kernelToString prefix) 0x5F#Word8 {-_-} (N.toString name)


toPrefixKey :: Module.Prefix -> N.Name -> Key
toPrefixKey prefix name =
  Key $ N.fromString $
    S.join (Module.prefixToString prefix) 0x5F#Word8 {-_-} (N.toString name)


hasDot :: Module.Name -> Bool
hasDot home =
    loop 0#
  where
    !(S.String ba) = Module.toString home
    !len = sizeofByteArray# ba

    loop i =
      if isTrue# (i <# len) then
        if isTrue# (eqWord8# 0x2E#Word8 (indexWord8Array# ba i))
        then True
        else loop (i +# 1#)
      else
        False


toNames :: Src.Exposing -> [N.Name]
toNames exposing =
  case exposing of
    Src.Open ->
      $(Crash.crash 'toNames) "cannot have `exposing (..)` in kernel code."

    Src.Explicit exposedList ->
      map toName exposedList


toName :: Src.Exposed -> N.Name
toName exposed =
  case exposed of
    Src.Lower n                -> A.toValue n
    Src.Upper n  Src.Private   -> T.nameToName (A.toValue n)
    Src.Upper _ (Src.Public _) -> $(Crash.crash 'toName) "cannot have Maybe(..) syntax in kernel code header"
    Src.Operator _ _           -> $(Crash.crash 'toName) "cannot use binops in kernel code"



-- BINARY


eChunk :: Chunk -> E.Builder
eChunk chunk =
  case chunk of
    JS     c   -> E.u8# 0#Word8 <> E.byteString64 c
    ElmVar h n -> E.u8# 1#Word8 <> ModuleName.eCanonical h <> N.encode n
    JsVar  h n -> E.u8# 2#Word8 <> Module.eKernel h <> N.encode n
    ElmField f -> E.u8# 3#Word8 <> N.encode f
    JsField  f -> E.u8# 4#Word8 <> E.int f
    JsEnum   i -> E.u8# 5#Word8 <> E.int i
    Debug      -> E.u8# 6#Word8
    Prod       -> E.u8# 7#Word8


dChunk :: D.Decoder Chunk
dChunk =
  do  tag <- D.u8
      case tag of
        0 -> liftM  JS D.byteString64
        1 -> liftM2 ElmVar ModuleName.dCanonical N.decode
        2 -> liftM2 JsVar Module.dKernel N.decode
        3 -> liftM  ElmField N.decode
        4 -> liftM  JsField D.int
        5 -> liftM  JsEnum D.int
        6 -> return Debug
        7 -> return Prod
        _ -> D.expecting "Elm.Kernel.Chunk"

