{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, MultiWayIf,
OverloadedStrings, TemplateHaskell, UnboxedTuples
#-}
module Elm.Docs
  ( Documentation
  , Module(..)
  , fromModule
  , Union(..)
  , Alias(..)
  , Value(..)
  , Binop(..)
  , Op.Associativity(..)
  , Op.Precedence(..)
  , Error(..)
  , decoder
  , encode
  )
  where


import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OOM
import qualified Data.Utf8 as Utf8
import GHC.Exts (isTrue#)
import GHC.Prim

import qualified Crash
import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified AST.Prim.Variable as Var
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.ModuleName as ModuleName
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Json.String as Json
import Parse.Primitives (Cursor, word1)
import qualified Parse.Primitives as P
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Docs as E
import qualified Reporting.Result as Result



-- DOCUMENTATION


type Documentation =
  Map.Map Module.Name Module


data Module =
  Module
    { _name :: Module.Name
    , _comment :: Comment
    , _unions :: Map.Map T.Name Union
    , _aliases :: Map.Map T.Name Alias
    , _values :: Map.Map N.Name Value
    , _binops :: Map.Map Op.Name Binop
    }

type Comment = Json.String

data Alias = Alias Comment [T.Var] Type.Type
data Union = Union Comment [T.Var] [(N.Name, [Type.Type])]
data Value = Value Comment Type.Type
data Binop = Binop Comment Type.Type Op.Associativity Op.Precedence



-- FROM MODULE


fromModule :: Can.Module -> IO (Either E.Error Module)
fromModule modul@(Can.Module _ exports docs _ _ _ _ _) =
  case exports of
    Can.ExportEverything region ->
      return $ Left $ E.ImplicitExposing region

    Can.Export ts vs bs ->
      case docs of
        Src.NoDocs region ->
          return $ Left $ E.NoDocs region

        Src.YesDocs overview vcs tcs ->
          do  result <- parseOverview overview
              return $
                do  names <- result
                    checkNames ts vs bs names
                    checkDefs ts vs bs overview (Map.fromList vcs) (Map.fromList tcs) modul




--------------------------------------------------------------------------------
-- JSON ------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- JSON


encode :: Documentation -> E.Value
encode docs =
  E.list encodeModule (Map.elems docs)


encodeModule :: Module -> E.Value
encodeModule (Module name comment unions aliases values binops) =
  E.object $
    [ "name"    ==> Module.jsonEncodeName name
    , "comment" ==> E.jsonString comment
    , "unions"  ==> E.list eUnion (Map.toList unions)
    , "aliases" ==> E.list eAlias (Map.toList aliases)
    , "values"  ==> E.list eValue (Map.toList values)
    , "binops"  ==> E.list eBinop (Map.toList binops)
    ]


data Error
  = BadAssociativity
  | BadModuleName
  | BadType


decoder :: D.Decoder Error Documentation
decoder =
  toDict <$> D.list moduleDecoder


toDict :: [Module] -> Documentation
toDict modules =
  Map.fromList (map toDictHelp modules)


toDictHelp :: Module -> (Module.Name, Module)
toDictHelp modul@(Module name _ _ _ _ _) =
  (name, modul)


moduleDecoder :: D.Decoder Error Module
moduleDecoder =
  Module
    <$> D.field "name" (Module.jsonDecodeName (\_ -> BadModuleName))
    <*> D.field "comment" D.jsonString
    <*> D.field "unions"  (dDict T.nameFromString dUnion)
    <*> D.field "aliases" (dDict T.nameFromString dAlias)
    <*> D.field "values"  (dDict N.fromString dValue)
    <*> D.field "binops"  (dDict Op.fromString dBinop)


dDict :: (Ord k) => (S.String -> k) -> D.Decoder Error a -> D.Decoder Error (Map.Map k a)
dDict toKey dEntry =
  Map.fromList <$> D.list (dNamed toKey dEntry)


dNamed :: (S.String -> k) -> D.Decoder Error a -> D.Decoder Error (k, a)
dNamed toKey dEntry =
  (,)
    <$> D.field "name" (dString toKey)
    <*> dEntry


dType :: D.Decoder Error Type.Type
dType =
  D.mapError (const BadType) Type.decoder



-- UNION JSON


eUnion :: (T.Name, Union) -> E.Value
eUnion (name, Union comment args cases) =
  E.object
    [ "name" ==> eString T.nameToString name
    , "comment" ==> E.jsonString comment
    , "args" ==> E.list (eString T.varToString) args
    , "cases" ==> E.list eCase cases
    ]


dUnion :: D.Decoder Error Union
dUnion =
  Union
    <$> D.field "comment" D.jsonString
    <*> D.field "args" (D.list (dString T.varFromString))
    <*> D.field "cases" (D.list dCase)


eCase :: (N.Name, [Type.Type]) -> E.Value
eCase (tag, args) =
  E.list id [ eString N.toString tag, E.list Type.encode args ]


dCase :: D.Decoder Error (N.Name, [Type.Type])
dCase =
  D.pair (dString N.fromString) (D.list dType)



-- ALIAS JSON


eAlias :: (T.Name, Alias) -> E.Value
eAlias (name, Alias comment args tipe) =
  E.object
    [ "name" ==> eString T.nameToString name
    , "comment" ==> E.jsonString comment
    , "args" ==> E.list (eString T.varToString) args
    , "type" ==> Type.encode tipe
    ]


dAlias :: D.Decoder Error Alias
dAlias =
  Alias
    <$> D.field "comment" D.jsonString
    <*> D.field "args" (D.list (dString T.varFromString))
    <*> D.field "type" dType



-- VALUE JSON


eValue :: (N.Name, Value) -> E.Value
eValue (name, Value comment tipe) =
  E.object
    [ "name" ==> eString N.toString name
    , "comment" ==> E.jsonString comment
    , "type" ==> Type.encode tipe
    ]


dValue :: D.Decoder Error Value
dValue =
  Value
    <$> D.field "comment" D.jsonString
    <*> D.field "type" dType



-- BINOP JSON


eBinop :: (Op.Name, Binop) -> E.Value
eBinop (name, Binop comment tipe assoc prec) =
  E.object
    [ "name" ==> eString Op.toString name
    , "comment" ==> E.jsonString comment
    , "type" ==> Type.encode tipe
    , "associativity" ==> eAssoc assoc
    , "precedence" ==> ePrec prec
    ]


dBinop :: D.Decoder Error Binop
dBinop =
  Binop
    <$> D.field "comment" D.jsonString
    <*> D.field "type" dType
    <*> D.field "associativity" dAssoc
    <*> D.field "precedence" dPrec



-- ASSOCIATIVITY JSON


eAssoc :: Op.Associativity -> E.Value
eAssoc assoc =
  case assoc of
    Op.Left  -> E.chars "left"
    Op.Non   -> E.chars "non"
    Op.Right -> E.chars "right"


dAssoc :: D.Decoder Error Op.Associativity
dAssoc =
  let
    left  = Json.fromChars "left"
    non   = Json.fromChars "non"
    right = Json.fromChars "right"
  in
  do  str <- D.jsonString
      if  | str == left  -> return Op.Left
          | str == non   -> return Op.Non
          | str == right -> return Op.Right
          | otherwise    -> D.failure BadAssociativity



-- PRECEDENCE JSON


ePrec :: Op.Precedence -> E.Value
ePrec (Op.Precedence n) =
  E.int (fromIntegral n)


dPrec :: D.Decoder Error Op.Precedence
dPrec =
  Op.Precedence . fromIntegral <$> D.int



-- STRING JSON


eString :: (a -> S.String) -> a -> E.Value
eString toStr a =
  E.string (toStr a)


dString :: (S.String -> a) -> D.Decoder x a
dString fromStr =
  do  (Utf8.Utf8 ba) <- D.jsonString
      pure $ fromStr $ S.String ba



--------------------------------------------------------------------------------
-- PARSE -----------------------------------------------------------------------
--------------------------------------------------------------------------------



-- PARSE OVERVIEW


data DocName
  = DocType  (A.Located T.Name)
  | DocValue (A.Located N.Name)
  | DocBinop (A.Located Op.Name)


parseOverview :: Src.Comment -> IO (Either E.Error [DocName])
parseOverview (Src.Comment snippet) =
  do  result <- P.fromSnippet (chompOverview []) E.BadEnd snippet
      case result of
        Right a -> return $ Right a
        Left  x -> return $ Left $ E.SyntaxProblem x


type Parser a =
  P.Parser E.SyntaxProblem a


chompOverview :: [DocName] -> Parser [DocName]
chompOverview names =
  do  isDocs <- chompUntilDocs
      if isDocs
        then
          do  Space.chomp E.Space
              chompOverview =<< chompDocs names
        else
          return names


chompDocs :: [DocName] -> Parser [DocName]
chompDocs names =
  do  start <- P.getPosition
      name <-
        P.oneOf E.Name
          [ DocValue <$> (Var.lower N.fromAddr     E.Name >>= P.addEnd start)
          , DocType  <$> (Var.upper T.nameFromAddr E.Name >>= P.addEnd start)
          , DocBinop <$> (chompOperator                   >>= P.addEnd start)
          ]

      Space.chomp E.Space

      P.oneOfWithFallback
        [ do  pos <- P.getPosition
              Space.checkIndent pos E.Comma
              word1 0x2C#Word8 {-,-} E.Comma
              Space.chomp E.Space
              chompDocs (name:names)
        ]
        (name:names)


chompOperator :: Parser Op.Name
chompOperator =
  do  word1 0x28#Word8 {-(-} E.Op
      op <- Symbol.operator E.Op E.OpBad
      word1 0x29#Word8 {-)-} E.Op
      return op


-- TODO add rule that @docs must be after newline in 0.20
--
chompUntilDocs :: Parser Bool
chompUntilDocs =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ _ ->
    let
      !(# isDocs, newPos, newCur #) = untilDocs pos end cur
      !newState = P.State newPos end indent newCur
    in
    cok isDocs newState


untilDocs :: Addr# -> Addr# -> Cursor -> (# Bool, Addr#, Cursor #)
untilDocs pos end cur =
  if P.notLtAddr pos end then
    (# False, pos, cur #)
  else
    case indexWord8OffAddr# pos 0# of
      0x0A#Word8 {-\n-} ->
        untilDocs (plusAddr# pos 1#) end (P.newline cur)

      word ->
        let !pos5 = plusAddr# pos 5# in
        if P.ltAddr pos5 end
          && P.eqIndex pos 0# 0x40#Word8 {-@-}
          && P.eqIndex pos 1# 0x64#Word8 {-d-}
          && P.eqIndex pos 2# 0x6F#Word8 {-o-}
          && P.eqIndex pos 3# 0x63#Word8 {-c-}
          && P.eqIndex pos 4# 0x73#Word8 {-s-}
          && P.eqAddr pos5 (Var.chompInner pos5 end (indexWord8OffAddr# pos5 0#))
        then
          (# True, pos5, P.slide cur 5#Word64 #)
        else
          let !newPos = plusAddr# pos (getCharWidth word) in
          untilDocs newPos end (P.slide cur 1#Word64)


getCharWidth :: Word8# -> Int#
getCharWidth word
  | isTrue# (ltWord8# word 0x80#Word8) = 1#
  | isTrue# (ltWord8# word 0xc0#Word8) = $(Crash.crash 'getCharWidth) "Need UTF-8 encoded input. Ran into unrecognized bits."
  | isTrue# (ltWord8# word 0xe0#Word8) = 2#
  | isTrue# (ltWord8# word 0xf0#Word8) = 3#
  | isTrue# (ltWord8# word 0xf8#Word8) = 4#
  | True                               = $(Crash.crash 'getCharWidth) "Need UTF-8 encoded input. Ran into unrecognized bits."




--------------------------------------------------------------------------------
-- VALIDATE --------------------------------------------------------------------
--------------------------------------------------------------------------------



-- CHECK NAMES


checkNames :: Map.Map T.Name (A.Region, Can.ExportType) -> Map.Map N.Name A.Region -> Map.Map Op.Name A.Region -> [DocName] -> Either E.Error ()
checkNames types values binops docNames0 =
  case snd $ Result.run $ loop docNames0 Map.empty Map.empty Map.empty of
    Right a -> Right a
    Left xs -> Left $ E.NameProblems $ OOM.destruct NE.List xs
  where
    loop docNames ts vs bs =
      case docNames of
        [] ->
          pure ()
            <* merge E.NameDuplicate_Type E.NameOnlyInDocs_Type E.NameOnlyInExports_Type fst types  ts
            <* merge E.NameDuplicate_Var  E.NameOnlyInDocs_Var  E.NameOnlyInExports_Var  id  values vs
            <* merge E.NameDuplicate_Op   E.NameOnlyInDocs_Op   E.NameOnlyInExports_Op   id  binops bs

        d:ds ->
          case d of
            DocType  (A.At r n) -> loop ds (add n r ts) vs bs
            DocValue (A.At r n) -> loop ds ts (add n r vs) bs
            DocBinop (A.At r n) -> loop ds ts vs (add n r bs)

    add n r dict =
      Map.insertWith OOM.more n (OOM.one r) dict

    merge errDup errDoc errExport toRegion exports docs =
      Map.mergeA
        (Map.traverseMissing      (\n e     -> Result.throw $ errExport n (toRegion e)))
        (Map.traverseMissing      (\n   oom -> onlyInDocs errDup errDoc n oom))
        (Map.zipWithMaybeAMatched (\n _ oom -> isUnique errDup n oom $> Nothing))
        exports
        docs


isUnique :: (k -> A.Region -> A.Region -> x) -> k -> OOM.OneOrMore A.Region -> Result.Result i w x A.Region
isUnique errDup name regions =
  case regions of
    OOM.One r ->
      Result.ok r

    OOM.More left right ->
      let (r1, r2) = OOM.getFirstTwo left right in
      Result.throw (errDup name r1 r2)


onlyInDocs :: (k -> A.Region -> A.Region -> x) -> (k -> A.Region -> x) -> k -> OOM.OneOrMore A.Region -> Result.Result i w x a
onlyInDocs errDup errDoc name regions =
  do  region <- isUnique errDup name regions
      Result.throw $ errDoc name region



-- CHECK DEFS


checkDefs :: Map.Map T.Name (A.Region, Can.ExportType) -> Map.Map N.Name A.Region -> Map.Map Op.Name A.Region -> Src.Comment -> Map.Map N.Name Src.Comment -> Map.Map T.Name Src.Comment -> Can.Module -> Either E.Error Module
checkDefs exportTypes exportValues exportBinops (Src.Comment overview) vComments tComments (Can.Module (ModuleName.Canonical _ name) _ _ decls unions aliases binops _) =
  case snd $ Result.run checker of
    Right a -> Right a
    Left xs -> Left $ E.DefProblems (OOM.destruct NE.List xs)
  where
    env =
      Env tComments unions aliases vComments (gatherTypes decls Map.empty)

    checker =
      (\ts vs bs ->
          let (us,as) = Map.mapEither id ts in
          Module name (Json.fromComment overview) us as vs bs
      )
        <$> Map.traverseWithKey (checkType  env) exportTypes
        <*> Map.traverseWithKey (checkValue env) exportValues
        <*> Map.traverseWithKey (checkBinop env binops) exportBinops


data Env =
  Env
    { _t_comments :: Map.Map T.Name Src.Comment
    , _t_unions   :: Map.Map T.Name Can.Union
    , _t_aliases  :: Map.Map T.Name Can.Alias
    , _v_comments :: Map.Map N.Name Src.Comment
    , _v_types    :: Map.Map N.Name (Either A.Region Can.Type)
    }


checkValue :: Env -> N.Name -> A.Region -> Result.Result x w E.DefProblem Value
checkValue (Env _ _ _ comments types) name region =
  do  tipe    <- getType name types (E.NoAnnotation_Var name)
      comment <- getComment region name comments E.NoComment_Var
      Result.ok $ Value comment tipe


checkType :: Env -> T.Name -> (A.Region, Can.ExportType) -> Result.Result i w E.DefProblem (Either Union Alias)
checkType (Env comments unions aliases _ _) name (region, export) =
  do  comment <- getComment region name comments E.NoComment_Type
      Result.ok $
        case export of
          Can.ExportAlias ->
            let (Can.Alias tvars tipe) = $(Map.require 'checkType) name aliases T.nameToChars in
            Right $ Alias comment tvars (Extract.fromType tipe)

          Can.ExportUnionOpen ->
            let (Can.Union tvars ctors _ _) = $(Map.require 'checkType) name unions T.nameToChars in
            Left $ Union comment tvars (map dector ctors)

          Can.ExportUnionClosed ->
            let (Can.Union tvars _ _ _) = $(Map.require 'checkType) name unions T.nameToChars in
            Left $ Union comment tvars []


checkBinop :: Env -> Map.Map Op.Name Can.Binop -> Op.Name -> A.Region -> Result.Result x w E.DefProblem Binop
checkBinop (Env _ _ _ comments types) binops op region =
  do  let (Can.Binop_ assoc prec name) = $(Map.require 'checkBinop) op binops Op.toChars
      tipe    <- getType name types (E.NoAnnotation_Op op)
      comment <- getComment region name comments E.NoComment_Var
      Result.ok $ Binop comment tipe assoc prec


getComment :: (Ord k) => A.Region -> k -> Map.Map k Src.Comment -> (k -> A.Region -> x) -> Result.Result i w x Comment
getComment r n comments toErr =
  case Map.lookup n comments of
    Just (Src.Comment s) -> Result.ok (Json.fromComment s)
    Nothing              -> Result.throw (toErr n r)


getType :: N.Name -> Map.Map N.Name (Either A.Region Can.Type) -> (A.Region -> x) -> Result.Result i w x Type.Type
getType name types toErr =
  case $(Map.require 'getType) name types N.toChars of
    Right t -> Result.ok (Extract.fromType t)
    Left  r -> Result.throw (toErr r)


dector :: Can.Ctor -> (N.Name, [Type.Type])
dector (Can.Ctor name _ _ args) =
  ( name, map Extract.fromType args )



-- GATHER TYPES


type Types =
  Map.Map N.Name (Either A.Region Can.Type)


gatherTypes :: Can.Decls -> Types -> Types
gatherTypes decls types =
  case decls of
    Can.Declare def subDecls ->
      gatherTypes subDecls (addDef types def)

    Can.DeclareRec def defs subDecls ->
      gatherTypes subDecls (List.foldl' addDef (addDef types def) defs)

    Can.SaveTheEnvironment ->
      types


addDef :: Types -> Can.Def -> Types
addDef types def =
  case def of
    Can.Def (A.At region name) _ _ ->
      Map.insert name (Left region) types

    Can.TypedDef (A.At _ name) _ typedArgs _ resultType ->
      let
        tipe = foldr Can.TLambda resultType (map snd typedArgs)
      in
      Map.insert name (Right tipe) types
