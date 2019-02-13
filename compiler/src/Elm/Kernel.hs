{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, OverloadedStrings, UnboxedTuples #-}
module Elm.Kernel
  ( Content(..)
  , Chunk(..)
  , Dep(..)
  , parser
  )
  where


import Control.Monad (liftM, liftM2)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr)

import qualified AST.Source as Src
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Parse.Module as Module
import qualified Parse.Variable as Var
import Parse.Utils
import Parse.Primitives hiding (Parser, State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- CONTENT


data Content =
  Content [Chunk] (Set.Set Dep)


data Dep
  = ElmDep ModuleName.Canonical Name.Name
  | KernelDep Name.Name
  deriving (Eq, Ord)



-- CHUNKS


data Chunk
  = JS (Utf8.VeryLong UNCHECKED)
  | ElmVar ModuleName.Canonical Name.Name
  | JsVar Name.Name Name.Name
  | ElmField Name.Name
  | JsField Int
  | JsEnum Int
  | Debug
  | Prod


data UNCHECKED



-- PARSE


type ImportDict =
  Map.Map Name.Name [Pkg.Name]


parser :: ImportDict -> Parser Content
parser importDict =
  do  word2 0x2F 0x2A {-/*-} E.XXX
      Module.freshLine
      imports <- Module.chompImports []
      word2 0x2A 0x2F {-*/-} E.XXX
      let (vtable, deps) = processImports importDict imports
      chunks <- parseChunks vtable Map.empty Map.empty
      return $ Content chunks deps



-- PARSE CHUNKS


parseChunks :: VarTable -> Enums -> Fields -> Parser [Chunk]
parseChunks vtable enums fields =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr _ ->
    let
      (# chunks, newPos, newRow, newCol #) =
        chompChunks vtable enums fields pos end row col pos []
    in
    if newPos == end then
      cok chunks (P.State newPos end indent newRow newCol ctx)
    else
      cerr row col ctx E.XXX


addJS :: Ptr Word8 -> Ptr Word8 -> [Chunk] -> [Chunk]
addJS start end revChunks =
  if start == end
    then revChunks
    else JS (Utf8.fromPtr start end) : revChunks


chompChunks :: VarTable -> Enums -> Fields -> Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> Ptr Word8 -> [Chunk] -> (# [Chunk], Ptr Word8, Word16, Word16 #)
chompChunks vs es fs pos end row col lastPos revChunks =
  if pos >= end then
    (# reverse (addJS lastPos end revChunks), pos, row, col #)

  else
    let !word = unsafeIndex pos in
    if word == 0x5F {-_-} then
      let
        !pos1 = plusPtr pos 1
        !pos3 = plusPtr pos 3
      in
      if pos3 <= end && unsafeIndex pos1 == 0x5F {-_-} then
        chompTag vs es fs pos3 end row (col + 3) (addJS lastPos pos revChunks)
      else
        chompChunks vs es fs pos1 end row (col + 1) lastPos revChunks

    else if word == 0x0A {-\n-} then
      chompChunks vs es fs (plusPtr pos 1) end (row + 1) 1 lastPos revChunks

    else
      let
        !newPos = plusPtr pos (getCharWidth pos end word)
      in
      chompChunks vs es fs newPos end row (col + 1) lastPos revChunks


-- relies on external checks in chompChunks
chompTag :: VarTable -> Enums -> Fields -> Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> [Chunk] -> (# [Chunk], Ptr Word8, Word16, Word16 #)
chompTag vs es fs pos end row col revChunks =
  let
    (# newPos, newCol #) = Var.chompInnerChars pos end col
    !tagPos = plusPtr pos (-1)
    !word = unsafeIndex tagPos
  in
  if word == 0x24 {-$-} then
    chompChunks vs es fs newPos end row newCol newPos $
      ElmField (Name.fromPtr pos newPos) : revChunks
  else
    let
      !name = Name.fromPtr tagPos newPos
    in
    if 0x30 {-0-} <= word && word <= 0x39 {-9-} then
      let
        (enum, newEnums) =
          lookupEnum (word - 0x30) name es
      in
      chompChunks vs newEnums fs newPos end row newCol newPos $
        JsEnum enum : revChunks

    else if 0x61 {-a-} <= word && word <= 0x7A {-z-} then
      let
        (field, newFields) =
          lookupField name fs
      in
      chompChunks vs es newFields newPos end row newCol newPos $
        JsField field : revChunks

    else if name == "DEBUG" then
      chompChunks vs es fs newPos end row newCol newPos (Debug : revChunks)

    else if name == "PROD" then
      chompChunks vs es fs newPos end row newCol newPos (Prod : revChunks)

    else
      case Map.lookup name vs of
        Just chunk ->
          chompChunks vs es fs newPos end row newCol newPos (chunk : revChunks)

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


type State =
  (VarTable, Set.Set Dep)


processImports :: ImportDict -> [Src.Import] -> State
processImports importDict imports =
  List.foldl' (addImport importDict) (Map.empty, Set.empty) imports


addImport :: ImportDict -> State -> Src.Import -> State
addImport importDict state (Src.Import (A.At _ home) maybeAlias exposing) =
  if Name.isKernel home then
    case maybeAlias of
      Just _ ->
        error ("Cannot use aliases on kernel import of: " ++ Name.toChars home)

      Nothing ->
        addKernelImport (Name.getKernel home) exposing state

  else
    case Map.lookup home importDict of
      Just [pkg] ->
        let
          canonicalHome = ModuleName.Canonical pkg home
          prefix = toPrefix home maybeAlias
        in
        addNormalImport canonicalHome prefix exposing state

      _ ->
        error ("Cannot find kernel import of: " ++ Name.toChars home)


-- INVARIANT: the `home` is the * in `Elm.Kernel.*`
--
addKernelImport :: Name.Name -> Src.Exposing -> State -> State
addKernelImport home exposing (vtable, deps) =
  let
    addVar table name =
      Map.insert (Name.sepBy 0x5F {-_-} home name) (JsVar home name) table
  in
  ( List.foldl' addVar vtable (toNames exposing)
  , Set.insert (KernelDep home) deps
  )


addNormalImport :: ModuleName.Canonical -> Name.Name -> Src.Exposing -> State -> State
addNormalImport home prefix exposing state =
  let
    addVar (vtable, deps) name =
      ( Map.insert (Name.sepBy 0x5F {-_-} prefix name) (ElmVar home name) vtable
      , Set.insert (ElmDep home name) deps
      )
  in
  List.foldl' addVar state (toNames exposing)


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
      error "Cannot have `exposing (..)` in kernel code."

    Src.Explicit exposedList ->
      map toName exposedList


toName :: A.Located Src.Exposed -> Name.Name
toName (A.At _ exposed) =
  case exposed of
    Src.Lower name ->
      name

    Src.Upper name Src.Private ->
      name

    Src.Upper _ Src.Public ->
      error "cannot have Maybe(..) syntax in kernel code header"

    Src.Operator _ ->
      error "cannot use binops in kernel code"



-- BINARY


instance Binary Chunk where
  put chunk =
    case chunk of
      JS a       -> putWord8 0 >> put (coerce a :: Utf8.String)
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
          0 -> liftM  toJS get
          1 -> liftM2 ElmVar get get
          2 -> liftM2 JsVar get get
          3 -> liftM  ElmField get
          4 -> liftM  JsField get
          5 -> liftM  JsEnum get
          6 -> return Debug
          7 -> return Prod
          _ -> error "problem deserializing Elm.Kernel.Chunk"


toJS :: Utf8.String -> Chunk
toJS str =
  JS (coerce str)
