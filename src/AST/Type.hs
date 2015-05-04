module AST.Type
    ( Raw, Raw'(..)
    , Canonical(..), Aliased(..)
    , Port(..), portType
    , deepDealias, dealias
    , collectLambdas
    , fieldMap
    , tuple
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map
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


portType :: Port tipe -> tipe
portType portType =
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
  pretty needsParens portType =
    case portType of
      Normal tipe ->
          P.pretty needsParens tipe

      Signal tipe _ ->
          P.pretty needsParens tipe


instance P.Pretty Raw' where
  pretty _ tipe =
    P.text (show tipe)


instance P.Pretty Canonical where
  pretty needsParens tipe =
    case tipe of
      Lambda _ _ ->
          P.parensIf needsParens $
              P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where
          t:ts =
              map prettyLambda (collectLambdas tipe)

          prettyLambda t =
              case t of
                Lambda _ _ ->
                    P.pretty True t
                _ ->
                    P.pretty False t

      Var x ->
          P.text x

      Type var ->
          let v = Var.toString var
          in
              P.text (if v == "_Tuple0" then "()" else v)

      App f args ->
          case (f,args) of
            (Type name, _)
                | Help.isTuple (Var.toString name) ->
                    P.parens $ P.sep $
                      P.punctuate P.comma (map (P.pretty False) args)

            (_, []) ->
                P.pretty True f

            (_, _) ->
                P.parensIf needsParens $
                  P.hang
                    (P.pretty True f)
                    2
                    (P.sep (map (P.pretty True) args))

      Record _ _ ->
          case flattenRecord tipe of
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
                      [ P.cat (zipWith (<+>) (P.space : repeat P.comma) (map prettyField fields))
                      , P.rbrace
                      ])
          where
            prettyField (field, tipe) =
                P.text field <+> P.text ":" <+> P.pretty False tipe

      Aliased name args _ ->
          P.parensIf (needsParens && not (null args)) $
            P.hang
              (P.pretty False name)
              2
              (P.sep (map (P.pretty True . snd) args))


collectLambdas :: Canonical -> [Canonical]
collectLambdas tipe =
  case tipe of
    Lambda arg body ->
        arg : collectLambdas body

    _ ->
        [tipe]


flattenRecord :: Canonical -> ( [(String, Canonical)], Maybe String )
flattenRecord tipe =
  case tipe of
    Var x ->
        ([], Just x)

    Record fields Nothing ->
        (fields, Nothing)

    Record fields (Just ext) ->
        let (fields',ext') = flattenRecord ext
        in
            (fields' ++ fields, ext')

    Aliased _ args tipe' ->
        flattenRecord (dealias args tipe')

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
