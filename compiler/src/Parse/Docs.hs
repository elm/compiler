module Parse.Docs
  ( Docs(..)
  , parser
  )
  where



-- DOCS


data Docs =
  Docs
    { _exposed  :: [A.Located Name.Name]
    , _overview :: Utf8.String
    , _listed   :: [A.Located Name.Name]
    , _comments :: Map.Map Name.Name Utf8.String
    }



-- PARSER


parser :: Parser Docs
parser =
  error "TODO implement docs parser"


chompUntilDocs :: Parser Bool
chompUntilDocs =
  P.Parser $ \(P.State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# isStart, newOffset, newRow, newCol #) =
        eatDocs fp offset terminal row col

      !newState =
        P.State fp newOffset terminal indent newRow newCol ctx
    in
    cok isStart newState


eatDocs :: ForeignPtr Word8 -> Int -> Int -> Word16 -> Word16 -> (# Bool, Int, Word16, Word16 #)
eatDocs fp offset terminal row col =
  if offset >= terminal then
    (# False, offset, row, col #)

  else if isDocsStart fp offset terminal then
    (# True, offset + 5, row, col + 5 #)

  else
    let !word = P.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatDocs fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + P.getCharWidth fp offset terminal word in
      eatDocs fp newOffset terminal row (col + 1)


isDocsStart :: ForeignPtr Word8 -> Int -> Int -> Bool
isDocsStart =
  let (B.PS dfp doff dlen) = docsStart in
  P.isSubstring dfp doff dlen


{-# NOINLINE docsStart #-}
docsStart :: B.ByteString
docsStart =
  "@docs"


chompOverview :: [A.Located Name.Name] -> Parser [A.Located Name.Name]
chompOverview names =
  do  isDocs <- chompUntilDocs
      if isDocs
        then chompOverviewHelp names
        else return names


chompOverviewHelp :: [A.Located Name.Name] -> Parser [A.Located Name.Name]
chompOverviewHelp names =
  do  pos <- getPosition
      (SPos spos) <- whitespace
      if pos == spos
        then chompOverview names
        else chompOverview =<< chompDocs names


chompDocs :: [A.Located Name.Name] -> Parser [A.Located Name.Name]
chompDocs names =
  do  name <- addLocation (oneOf SE.XXX [ Var.lower, Var.upper, chompBinop ])
      spos <- whitespace
      oneOf SE.XXX
        [ do  checkSpace spos
              word1 0x2C {-,-} SE.XXX
              spaces
              chompDocs (name:names)
        , return (name:names)
        ]


chompBinop :: Parser Name.Name
chompBinop =
  do  word1 0x28 {-(-} SE.XXX
      name <- Symbol.binop
      word1 0x29 {-)-} SE.XXX
      return name




-- CHECK NAMES


type Result i w a =
  Result.Result i w E.Error a


type Dups =
  Map.Map Name.Name (OneOrMore.OneOrMore A.Region)


checkNames :: Map.Map Name.Name (A.Located Can.Export) -> [A.Located Name.Name] -> Result i w ()
checkNames exports names =
  do  docs <- Map.traverseWithKey isUnique (List.foldl' addName Map.empty names)
      let overlap = Map.size (Map.intersection docs exports)
      if Map.size exports == overlap && overlap == Map.size docs
        then Result.ok ()
        else
          do  _ <- Map.traverseWithKey onlyInDocs (Map.difference docs exports)
              _ <- Map.traverseWithKey onlyInExports (Map.difference exports docs)
              Result.ok ()


addName :: Dups -> A.Located Name.Name -> Dups
addName dict (A.At region name) =
  Map.insertWith OneOrMore.more name (OneOrMore.one region) dict


isUnique :: Name.Name -> OneOrMore.OneOrMore A.Region -> Result i w A.Region
isUnique name regions =
  case regions of
    OneOrMore.One region ->
      Result.ok region

    OneOrMore.More _ _ ->
      let (r1:r2:_) = OneOrMore.toList regions in
      Result.throw (E.Duplicate name r1 r2)


onlyInDocs :: Name.Name -> A.Region -> Result i w a
onlyInDocs name region =
  Result.throw $ E.OnlyInDocs name region


onlyInExports :: Name.Name -> A.Located Can.Export -> Result i w a
onlyInExports name (A.At region _) =
  Result.throw $ E.OnlyInExports name region



-- CHECK EXPORTS


data Info =
  Info
    { _iComments :: Map.Map Name.Name Comment
    , _iValues   :: Types
    , _iUnions   :: Map.Map Name.Name Can.Union
    , _iAliases  :: Map.Map Name.Name Can.Alias
    , _iBinops   :: Map.Map Name.Name Can.Binop
    , _iEffects  :: Can.Effects
    }


checkExport :: Info -> Name.Name -> A.Located Can.Export -> Result i w (Module -> Module)
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


getComment :: A.Region -> Name.Name -> Info -> Result i w Comment
getComment region name info =
  case Map.lookup name (_iComments info) of
    Nothing ->
      Result.throw (E.NoComment name region)

    Just comment ->
      Result.ok comment


getType :: Name.Name -> Info -> Result i w Type.Type
getType name info =
  case _iValues info ! name of
    A.At defRegion Nothing ->
      Result.throw (E.NoAnnotation name defRegion)

    A.At _ (Just tipe) ->
      Result.ok (Extract.fromType tipe)


dector :: Can.Ctor -> (Name.Name, [Type.Type])
dector (Can.Ctor name _ _ args) =
  ( name, map Extract.fromType args )



-- GATHER TYPES


type Types =
  Map.Map Name.Name (A.Located (Maybe Can.Type))


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
