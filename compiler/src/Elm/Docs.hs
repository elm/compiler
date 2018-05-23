{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs
  ( Documentation
  , toDict
  , Module(..)
  , fromModule
  , Union(..)
  , Alias(..)
  , Value(..)
  , Binop(..)
  , Binop.Associativity(..)
  , Binop.Precedence(..)
  , Error(..)
  , decoder
  , encode
  )
  where


import qualified Data.ByteString as B
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Name as N
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Docs as E
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import Parse.Primitives (Parser)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W



-- DOCUMENTATION


type Documentation =
  Map.Map N.Name Module


toDict :: [Module] -> Documentation
toDict modules =
  Map.fromList (map toPair modules)



toPair :: Module -> (N.Name, Module)
toPair modul@(Module name _ _ _ _ _) =
  (name, modul)



-- MODULES


data Module =
  Module
    { _name :: N.Name
    , _comment :: Comment
    , _unions :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _values :: Map.Map N.Name Value
    , _binops :: Map.Map N.Name Binop
    }


type Comment = Text.Text


data Alias = Alias Comment [N.Name] Type.Type
data Union = Union Comment [N.Name] [(N.Name, [Type.Type])]
data Value = Value Comment Type.Type
data Binop = Binop Comment Type.Type Binop.Associativity Binop.Precedence



-- JSON ENCODE / DECODE


encode :: Module -> E.Value
encode (Module name comment unions aliases values binops) =
  E.object $
    [ "name" ==> Module.encode name
    , "comment" ==> E.text comment
    , "unions" ==> E.list encodeUnion (Map.toList unions)
    , "aliases" ==> E.list encodeAlias (Map.toList aliases)
    , "values" ==> E.list encodeValue (Map.toList values)
    , "binops" ==> E.list encodeBinop (Map.toList binops)
    ]


data Error
  = BadAssociativity Text.Text
  | BadName
  | BadType


decoder :: D.Decoder Error Module
decoder =
  Module
    <$> D.field "name" nameDecoder
    <*> D.field "comment" D.text
    <*> D.field "unions" (dictDecoder union)
    <*> D.field "aliases" (dictDecoder alias)
    <*> D.field "values" (dictDecoder value)
    <*> D.field "binops" (dictDecoder binop)


dictDecoder :: D.Decoder Error a -> D.Decoder Error (Map.Map N.Name a)
dictDecoder entryDecoder =
  Map.fromList <$> D.list (named entryDecoder)


named :: D.Decoder Error a -> D.Decoder Error (N.Name, a)
named entryDecoder =
  (,)
    <$> D.field "name" D.name
    <*> entryDecoder


nameDecoder :: D.Decoder Error N.Name
nameDecoder =
  D.mapError (const BadName) Module.decoder


typeDecoder :: D.Decoder Error Type.Type
typeDecoder =
  D.mapError (const BadType) Type.decoder



-- UNION JSON


encodeUnion :: (N.Name, Union) -> E.Value
encodeUnion (name, Union comment args cases) =
  E.object
    [ "name" ==> E.name name
    , "comment" ==> E.text comment
    , "args" ==> E.list E.name args
    , "cases" ==> E.list encodeCase cases
    ]


union :: D.Decoder Error Union
union =
  Union
    <$> D.field "comment" D.text
    <*> D.field "args" (D.list D.name)
    <*> D.field "cases" (D.list caseDecoder)


encodeCase :: ( N.Name, [Type.Type] ) -> E.Value
encodeCase ( tag, args ) =
  E.list id [ E.name tag, E.list Type.encode args ]


caseDecoder :: D.Decoder Error ( N.Name, [Type.Type] )
caseDecoder =
  (,)
    <$> D.index 0 D.name
    <*> D.index 1 (D.list typeDecoder)



-- ALIAS JSON


encodeAlias :: (N.Name, Alias) -> E.Value
encodeAlias ( name, Alias comment args tipe) =
  E.object
    [ "name" ==> E.name name
    , "comment" ==> E.text comment
    , "args" ==> E.list E.name args
    , "type" ==> Type.encode tipe
    ]


alias :: D.Decoder Error Alias
alias =
  Alias
    <$> D.field "comment" D.text
    <*> D.field "args" (D.list D.name)
    <*> D.field "type" typeDecoder



-- VALUE JSON


encodeValue :: (N.Name, Value) -> E.Value
encodeValue (name, Value comment tipe) =
  E.object
    [ "name" ==> E.name name
    , "comment" ==> E.text comment
    , "type" ==> Type.encode tipe
    ]


value :: D.Decoder Error Value
value =
  Value
    <$> D.field "comment" D.text
    <*> D.field "type" typeDecoder



-- BINOP JSON


encodeBinop :: (N.Name, Binop) -> E.Value
encodeBinop (name, Binop comment tipe assoc prec) =
  E.object
    [ "name" ==> E.name name
    , "comment" ==> E.text comment
    , "type" ==> Type.encode tipe
    , "associativity" ==> encodeAssoc assoc
    , "precedence" ==> encodePrec prec
    ]


binop :: D.Decoder Error Binop
binop =
  Binop
    <$> D.field "comment" D.text
    <*> D.field "type" typeDecoder
    <*> D.field "associativity" assocDecoder
    <*> D.field "precedence" precDecoder



-- ASSOCIATIVITY JSON


encodeAssoc :: Binop.Associativity -> E.Value
encodeAssoc assoc =
  case assoc of
    Binop.Left  -> E.text "left"
    Binop.Non   -> E.text "non"
    Binop.Right -> E.text "right"


assocDecoder :: D.Decoder Error Binop.Associativity
assocDecoder =
  do  txt <- D.text
      case txt of
        "left"  -> D.succeed Binop.Left
        "non"   -> D.succeed Binop.Non
        "right" -> D.succeed Binop.Right
        _       -> D.fail (BadAssociativity txt)



-- PRECEDENCE JSON


encodePrec :: Binop.Precedence -> E.Value
encodePrec (Binop.Precedence n) =
  E.int n


precDecoder :: D.Decoder Error Binop.Precedence
precDecoder =
  Binop.Precedence <$> D.int



-- FROM MODULE


fromModule :: Can.Module -> Result.Result i w Error.Error Module
fromModule (Can.Module name docs exports decls unions aliases binops effects) =
  case exports of
    Can.ExportEverything region ->
      Result.throw $ Error.Docs $ E.ImplicitExposing region

    Can.Export exportDict ->
      case docs of
        Can.NoDocs region ->
          Result.throw $ Error.Docs $ E.NoDocs region

        Can.YesDocs region overview comments ->
          do  names <- parseOverview region overview
              Result.mapError Error.Docs $
                do  checkNames exportDict names
                    let types = gatherTypes decls Map.empty
                    let info = Info comments types unions aliases binops effects
                    inserters <- Map.traverseWithKey (checkExport info) exportDict
                    return $ foldr ($) (emptyModule name overview) inserters


emptyModule :: ModuleName.Canonical -> B.ByteString -> Module
emptyModule (ModuleName.Canonical _ name) overview =
  Module name (Text.decodeUtf8 overview) Map.empty Map.empty Map.empty Map.empty



-- PARSE OVERVIEW


parseOverview :: R.Region -> B.ByteString -> Result.Result i w Error.Error [A.Located N.Name]
parseOverview (R.Region (R.Position row col) _) overview =
  case P.runAt row (col + 3) (chompOverview []) overview of
    Left err ->
      Result.throw (Error.Syntax err)

    Right names ->
      Result.ok names


chompOverview :: [A.Located N.Name] -> Parser [A.Located N.Name]
chompOverview names =
  do  isDocs <- W.chompUntilDocs
      if isDocs
        then chompOverviewHelp names
        else return names


chompOverviewHelp :: [A.Located N.Name] -> Parser [A.Located N.Name]
chompOverviewHelp names =
  do  pos <- P.getPosition
      (W.SPos spos) <- W.whitespace
      if pos == spos
        then chompOverview names
        else chompOverview =<< chompDocs names


chompDocs :: [A.Located N.Name] -> Parser [A.Located N.Name]
chompDocs names =
  do  name <- P.addLocation (P.oneOf [ Var.lower, Var.upper, chompBinop ])
      spos <- W.whitespace
      P.oneOf
        [ do  P.checkSpace spos
              Symbol.comma
              P.spaces
              chompDocs (name:names)
        , return (name:names)
        ]


chompBinop :: Parser N.Name
chompBinop =
  do  Symbol.leftParen
      name <- Symbol.binop
      Symbol.rightParen
      return name



-- CHECK NAMES


type Result i w a =
  Result.Result i w E.Error a


type Dups =
  Map.Map N.Name (OneOrMore.OneOrMore R.Region)


checkNames :: Map.Map N.Name (A.Located Can.Export) -> [A.Located N.Name] -> Result i w ()
checkNames exports names =
  do  docs <- Map.traverseWithKey isUnique (List.foldl' addName Map.empty names)
      let overlap = Map.size (Map.intersection docs exports)
      if Map.size exports == overlap && overlap == Map.size docs
        then Result.ok ()
        else
          do  _ <- Map.traverseWithKey onlyInDocs (Map.difference docs exports)
              _ <- Map.traverseWithKey onlyInExports (Map.difference exports docs)
              Result.ok ()


addName :: Dups -> A.Located N.Name -> Dups
addName dict (A.At region name) =
  Map.insertWith OneOrMore.more name (OneOrMore.one region) dict


isUnique :: N.Name -> OneOrMore.OneOrMore R.Region -> Result i w R.Region
isUnique name regions =
  case regions of
    OneOrMore.One region ->
      Result.ok region

    OneOrMore.More _ _ ->
      let (r1:r2:_) = OneOrMore.toList regions in
      Result.throw (E.Duplicate name r1 r2)


onlyInDocs :: N.Name -> R.Region -> Result i w a
onlyInDocs name region =
  Result.throw $ E.OnlyInDocs name region


onlyInExports :: N.Name -> A.Located Can.Export -> Result i w a
onlyInExports name (A.At region _) =
  Result.throw $ E.OnlyInExports name region



-- CHECK EXPORTS


data Info =
  Info
    { _iComments :: Map.Map N.Name Comment
    , _iValues   :: Types
    , _iUnions   :: Map.Map N.Name Can.Union
    , _iAliases  :: Map.Map N.Name Can.Alias
    , _iBinops   :: Map.Map N.Name Can.Binop
    , _iEffects  :: Can.Effects
    }


checkExport :: Info -> N.Name -> A.Located Can.Export -> Result i w (Module -> Module)
checkExport info name (A.At region export) =
  case export of
    Can.ExportValue ->
      do  tipe <- getType name info
          comment <- getComment region name info
          Result.ok $ \m ->
            m { _values = Map.insert name (Value comment tipe) (_values m) }

    Can.ExportBinop ->
      do  let (Can.Binop_ assoc prec realName) = _iBinops info ! name
          tipe <- getType realName info
          comment <- getComment region realName info
          Result.ok $ \m ->
            m { _binops = Map.insert name (Binop comment tipe assoc prec) (_binops m) }

    Can.ExportAlias ->
      do  let (Can.Alias tvars tipe) = _iAliases info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { _aliases = Map.insert name (Alias comment tvars (Extract.fromType tipe)) (_aliases m) }

    Can.ExportUnionOpen ->
      do  let (Can.Union tvars ctors _ _) = _iUnions info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { _unions = Map.insert name (Union comment tvars (map dector ctors)) (_unions m) }

    Can.ExportUnionClosed ->
      do  let (Can.Union tvars _ _ _) = _iUnions info ! name
          comment <- getComment region name info
          Result.ok $ \m ->
            m { _unions = Map.insert name (Union comment tvars []) (_unions m) }

    Can.ExportPort ->
      do  tipe <- getType name info
          comment <- getComment region name info
          Result.ok $ \m ->
            m { _values = Map.insert name (Value comment tipe) (_values m) }


getComment :: R.Region -> N.Name -> Info -> Result i w Comment
getComment region name info =
  case Map.lookup name (_iComments info) of
    Nothing ->
      Result.throw (E.NoComment name region)

    Just comment ->
      Result.ok comment


getType :: N.Name -> Info -> Result i w Type.Type
getType name info =
  case _iValues info ! name of
    A.At defRegion Nothing ->
      Result.throw (E.NoAnnotation name defRegion)

    A.At _ (Just tipe) ->
      Result.ok (Extract.fromType tipe)


dector :: Can.Ctor -> (N.Name, [Type.Type])
dector (Can.Ctor name _ _ args) =
  ( name, map Extract.fromType args )



-- GATHER TYPES


type Types =
  Map.Map N.Name (A.Located (Maybe Can.Type))


gatherTypes :: Can.Decls -> Types -> Types
gatherTypes decls types =
  case decls of
    Can.Declare def subDecls ->
      gatherTypes subDecls (addDef types def)

    Can.DeclareRec defs subDecls ->
      gatherTypes subDecls (List.foldl' addDef types defs)

    Can.SaveTheEnvironment ->
      types


addDef :: Types -> Can.Def -> Types
addDef types def =
  case def of
    Can.Def (A.At region name) _ _ ->
      Map.insert name (A.At region Nothing) types

    Can.TypedDef (A.At region name) _ typedArgs _ resultType ->
      let
        tipe =
          foldr Can.TLambda resultType (map snd typedArgs)
      in
      Map.insert name (A.At region (Just tipe)) types
