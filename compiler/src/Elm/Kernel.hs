{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Kernel
  ( Opt.KContent(..)
  , parse
  )
  where


import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word8)


import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import Parse.Primitives (Parser, run)
import qualified Parse.Primitives.Kernel as K
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Module as Module
import qualified Reporting.Annotation as A
import qualified Reporting.Error as E



-- PARSE


type ImportDict =
  Map.Map N.Name [Pkg.Name]


parse :: ImportDict -> B.ByteString -> Either E.Error Opt.KContent
parse importDict sourceCode =
  case run (parser importDict) sourceCode of
    Right kernel ->
      Right kernel

    Left err ->
      Left (E.Syntax err)


parser :: ImportDict -> Parser Opt.KContent
parser importDict =
  do  Symbol.jsMultiCommentOpen
      Module.freshLine
      imports <- Module.chompImports []
      Symbol.jsMultiCommentClose

      let (State vtable deps) = processImports importDict imports

      chunks <- chompChunks vtable Map.empty Map.empty []

      return $ Opt.KContent chunks deps



-- PROCESS IMPORTS


type VarTable =
  Map.Map N.Name Opt.KChunk


data State =
  State VarTable (Set.Set Opt.Global)


processImports :: ImportDict -> [Src.Import] -> State
processImports importDict imports =
  List.foldl' (addImport importDict) (State Map.empty Set.empty) imports


addImport :: ImportDict -> State -> Src.Import -> State
addImport importDict state (Src.Import (A.At _ home) maybeAlias exposing) =
  if ModuleName.isKernel home then
    case maybeAlias of
      Just _ ->
        error ("Cannot use aliases on kernel import of: " ++ N.toString home)

      Nothing ->
        addKernelImport (ModuleName.getKernel home) exposing state

  else
    case Map.lookup home importDict of
      Just [pkg] ->
        let
          canonicalHome = ModuleName.Canonical pkg home
          prefix = toPrefix home maybeAlias
        in
        addNormalImport canonicalHome prefix exposing state

      _ ->
        error ("Cannot find kernel import of: " ++ N.toString home)


-- INVARIANT: the `home` is the * in `Elm.Kernel.*`
--
addKernelImport :: N.Name -> Src.Exposing -> State -> State
addKernelImport home exposing (State vtable deps) =
  let
    addVar table name =
      Map.insert (N.sepBy 0x5F {- _ -} home name) (Opt.JsVar home name) table
  in
  State
    (List.foldl' addVar vtable (toNames exposing))
    (Set.insert (Opt.kernel home) deps)


addNormalImport :: ModuleName.Canonical -> N.Name -> Src.Exposing -> State -> State
addNormalImport home prefix exposing state =
  let
    addVar (State vtable deps) name =
      State
        (Map.insert (N.sepBy 0x5F {- _ -} prefix name) (Opt.ElmVar home name) vtable)
        (Set.insert (Opt.Global home name) deps)
  in
  List.foldl' addVar state (toNames exposing)


toPrefix :: N.Name -> Maybe N.Name -> N.Name
toPrefix home maybeAlias =
  case maybeAlias of
    Just alias ->
      alias

    Nothing ->
      if N.contains 0x2E {- . -} home then
        error ("kernel imports with dots need an alias: " ++ show (N.toString home))
      else
        home

toNames :: Src.Exposing -> [N.Name]
toNames exposing =
  case exposing of
    Src.Open ->
      error "Cannot have `exposing (..)` in kernel code."

    Src.Explicit exposedList ->
      map toName exposedList


toName :: A.Located Src.Exposed -> N.Name
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



-- PARSE CHUNKS


chompChunks :: VarTable -> Enums -> Fields -> [Opt.KChunk] -> Parser [Opt.KChunk]
chompChunks vtable enums fields chunks =
  do  (javascript, maybeTag) <- K.chunk
      case maybeTag of
        Nothing ->
          return (Opt.JS javascript : chunks)

        Just tag ->
          case tag of
            K.Prod ->
              chompChunks vtable enums fields $
                Opt.Prod : Opt.JS javascript : chunks

            K.Debug ->
              chompChunks vtable enums fields $
                Opt.Debug : Opt.JS javascript : chunks

            K.Import var ->
              case Map.lookup var vtable of
                Nothing ->
                  error ("Bad kernel symbol: " ++ N.toString var)

                Just chunk ->
                  chompChunks vtable enums fields $
                    chunk : Opt.JS javascript : chunks

            K.Enum n var ->
              let (enum, newEnums) = lookupEnum n var enums in
              chompChunks vtable newEnums fields $
                Opt.JsEnum enum : Opt.JS javascript : chunks

            K.ElmField name ->
              chompChunks vtable enums fields $
                Opt.ElmField name : Opt.JS javascript : chunks

            K.JsField name ->
              let (field, newFields) = lookupField name fields in
              chompChunks vtable enums newFields $
                Opt.JsField field : Opt.JS javascript : chunks



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
