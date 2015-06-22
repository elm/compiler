module AST.Type
    ( Raw, Raw'(..)
    , Canonical(..), Aliased(..)
    , Port(..), getPortType
    , deepDealias, dealias
    , collectLambdas
    , fieldMap
    , tuple
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.PrettyPrint as P

import qualified AST.Variable as Var
import qualified AST.Helpers as Help
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


-- DEFINITION

type Raw =
    A.Located Raw'


data Raw'
    = RLambda Raw Raw
    | RVar String
    | RType Var.Raw
    | RApp Raw [Raw]
    | RRecord [(String, Raw)] (Maybe Raw)
    deriving (Show)


data Canonical
    = Lambda Canonical Canonical
    | Var String
    | Type Var.Canonical
    | App Canonical [Canonical]
    | Record [(String, Canonical)] (Maybe Canonical)
    | Aliased Var.Canonical [(String, Canonical)] (Aliased Canonical)
    deriving (Eq, Ord, Show)


data Aliased t
    = Holey t
    | Filled t
    deriving (Eq, Ord, Show)


data Port t
    = Normal t
    | Signal { root :: t, arg :: t }
    deriving (Show)


getPortType :: Port tipe -> tipe
getPortType portType =
  case portType of
    Normal tipe -> tipe
    Signal tipe _ -> tipe


fieldMap :: [(String,a)] -> Map.Map String [a]
fieldMap fields =
  let add r (field,tipe) =
        Map.insertWith (++) field [tipe] r
  in
      foldl add Map.empty fields

{--
recordOf :: [(String, Type var)] -> Type var
recordOf fields =
  Record fields Nothing


listOf :: RawType -> RawType
listOf tipe =
  App (Type (Var.Raw "List")) [tipe]
--}

tuple :: R.Region -> [Raw] -> Raw
tuple region types =
  let name = Var.Raw ("_Tuple" ++ show (length types))
  in
      A.A region (RApp (A.A region (RType name)) types)


-- DEALIASING

deepDealias :: Canonical -> Canonical
deepDealias tipe =
  let go = deepDealias in
  case tipe of
    Lambda a b ->
          Lambda (go a) (go b)

    Var _ ->
        tipe

    Record fields ext ->
        Record (map (second go) fields) (fmap go ext)

    Aliased _name args tipe' ->
        deepDealias (dealias args tipe')

    Type _ ->
        tipe

    App f args ->
        App (go f) (map go args)


dealias :: [(String, Canonical)] -> Aliased Canonical -> Canonical
dealias args aliasType =
  case aliasType of
    Holey tipe ->
        dealiasHelp (Map.fromList args) tipe

    Filled tipe ->
        tipe


dealiasHelp :: Map.Map String Canonical -> Canonical -> Canonical
dealiasHelp typeTable tipe =
    let go = dealiasHelp typeTable in
    case tipe of
      Lambda a b ->
          Lambda (go a) (go b)

      Var x ->
          Map.findWithDefault tipe x typeTable

      Record fields ext ->
          Record (map (second go) fields) (fmap go ext)

      Aliased original args t' ->
          Aliased original (map (second go) args) t'

      Type _ ->
          tipe

      App f args ->
          App (go f) (map go args)


-- PRETTY PRINTING

instance (P.Pretty t) => P.Pretty (Port t) where
  pretty dealiaser needsParens portType =
    P.pretty dealiaser needsParens (getPortType portType)


instance P.Pretty Raw' where
  pretty dealiaser needsParens tipe =
    case tipe of
      RLambda arg body ->
          P.parensIf needsParens (prettyLambda dealiaser getRawLambda arg body)

      RVar x ->
          P.text x

      RType var ->
          prettyType dealiaser var

      RApp func args ->
          let
            isTuple (A.A _ (RType name)) = Help.isTuple (Var.toString name)
            isTuple _ = False
          in
            prettyApp dealiaser needsParens isTuple func args

      RRecord fields ext ->
          prettyRecord dealiaser (flattenRawRecord fields ext)


instance P.Pretty Canonical where
  pretty dealiaser needsParens tipe =
    case tipe of
      Lambda arg body ->
          P.parensIf needsParens (prettyLambda dealiaser getCanLambda arg body)

      Var x ->
          P.text x

      Type var ->
          prettyType dealiaser var

      App func args ->
          let
            isTuple (Type name) = Help.isTuple (Var.toString name)
            isTuple _ = False
          in
            prettyApp dealiaser needsParens isTuple func args

      Record fields ext ->
          prettyRecord dealiaser (flattenCanRecord fields ext)

      Aliased name args _ ->
          P.parensIf (needsParens && not (null args)) $
            P.hang
              (P.pretty dealiaser False name)
              2
              (P.sep (map (P.pretty dealiaser True . snd) args))


-- PRETTY HELPERS

prettyType :: (Var.ToString var) => P.Dealiaser -> var -> P.Doc
prettyType dealiaser var =
  let
    v = Var.toString var
  in
    P.text $
      if v == "_Tuple0" then
        "()"
      else
        maybe v id (Map.lookup v dealiaser)


-- PRETTY LAMBDAS

prettyLambda :: (P.Pretty t) => P.Dealiaser -> (t -> Maybe (t,t)) -> t -> t -> P.Doc
prettyLambda dealiaser getLambda arg body =
  let
    rest =
      gatherLambda getLambda body

    prettyArg t =
      P.pretty dealiaser (Maybe.isJust (getLambda t)) t
  in
    P.sep
      [ prettyArg arg
      , P.sep (map (\t -> P.text "->" <+> prettyArg t) rest)
      ]


getRawLambda :: Raw -> Maybe (Raw, Raw)
getRawLambda (A.A _ tipe) =
  case tipe of
    RLambda arg body -> Just (arg, body)
    _ -> Nothing


getCanLambda :: Canonical -> Maybe (Canonical, Canonical)
getCanLambda tipe =
  case tipe of
    Lambda arg body -> Just (arg, body)
    _ -> Nothing


gatherLambda :: (t -> Maybe (t,t)) -> t -> [t]
gatherLambda get tipe =
  case get tipe of
    Just (arg, body) ->
        arg : gatherLambda get body

    Nothing ->
        [tipe]


collectLambdas :: Canonical -> [Canonical]
collectLambdas tipe =
  gatherLambda getCanLambda tipe


-- PRETTY APP

prettyApp :: (P.Pretty t) => P.Dealiaser -> Bool -> (t -> Bool) -> t -> [t] -> P.Doc
prettyApp dealiaser needsParens isTuple func args
  | isTuple func =
        P.parens $ P.sep $
            P.punctuate P.comma (map (P.pretty dealiaser False) args)

  | null args =
      P.pretty dealiaser needsParens func

  | otherwise =
      P.parensIf needsParens $
        P.hang
          (P.pretty dealiaser True func)
          2
          (P.sep (map (P.pretty dealiaser True) args))


-- PRETTY RECORD

prettyRecord :: (P.Pretty t) => P.Dealiaser -> ([(String, t)], Maybe String) -> P.Doc
prettyRecord dealiaser recordInfo =
  let
    prettyField (field, tipe) =
      P.hang
          (P.text field <+> P.text ":")
          4
          (P.pretty dealiaser False tipe)
  in
  case recordInfo of
    ([], Nothing) ->
        P.text "{}"

    (fields, Nothing) ->
        P.sep
          [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map prettyField fields))
          , P.rbrace
          ]

    (fields, Just x) ->
        P.hang
            (P.lbrace <+> P.text x <+> P.text "|")
            4
            (P.sep
              [ P.sep (P.punctuate P.comma (map prettyField fields))
              , P.rbrace
              ]
            )


flattenRawRecord
    :: [(String, Raw)]
    -> Maybe Raw
    -> ( [(String, Raw)], Maybe String )
flattenRawRecord fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (A.A _ (RVar x)) ->
        (fields, Just x)

    Just (A.A _ (RRecord fields' ext')) ->
        flattenRawRecord (fields' ++ fields) ext'

    _ ->
        error "Trying to flatten ill-formed record."


flattenCanRecord
    :: [(String, Canonical)]
    -> Maybe Canonical
    -> ( [(String, Canonical)], Maybe String )
flattenCanRecord fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (Var x) ->
        (fields, Just x)

    Just (Record fields' ext') ->
        flattenCanRecord (fields' ++ fields) ext'

    Just (Aliased _ args tipe) ->
        flattenCanRecord fields (Just (dealias args tipe))

    _ ->
        error "Trying to flatten ill-formed record."


-- BINARY

instance Binary Canonical where
  put tipe =
      case tipe of
        Lambda t1 t2 ->
            putWord8 0 >> put t1 >> put t2

        Var x ->
            putWord8 1 >> put x

        Type name ->
            putWord8 2 >> put name

        App t1 t2 ->
            putWord8 3 >> put t1 >> put t2

        Record fs ext ->
            putWord8 4 >> put fs >> put ext

        Aliased var args t ->
            putWord8 5 >> put var >> put args >> put t

  get = do
      n <- getWord8
      case n of
        0 -> Lambda <$> get <*> get
        1 -> Var <$> get
        2 -> Type <$> get
        3 -> App <$> get <*> get
        4 -> Record <$> get <*> get
        5 -> Aliased <$> get <*> get <*> get
        _ -> error "Error reading a valid type from serialized string"


instance Binary t => Binary (Aliased t) where
  put aliasType =
      case aliasType of
        Holey tipe ->
            putWord8 0 >> put tipe

        Filled tipe ->
            putWord8 1 >> put tipe

  get = do
      n <- getWord8
      case n of
        0 -> Holey <$> get
        1 -> Filled <$> get
        _ -> error "Error reading a valid type from serialized string"
