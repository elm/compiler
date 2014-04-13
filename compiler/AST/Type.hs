{-# OPTIONS_GHC -W #-}
module AST.Type where

import Control.Applicative ((<$>), (<*>))
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Variable as Var
import AST.PrettyPrint
import qualified AST.Helpers as Help
import Text.PrettyPrint as P

data Type var
    = Lambda (Type var) (Type var)
    | Var String
    | Data var [Type var]
    | Record [(String, Type var)] (Maybe (Type var))
    | Aliased Var.Canonical (Type var)
    deriving (Eq,Show)

type RawType = Type Var.Raw
type CanonicalType = Type Var.Canonical

fieldMap :: [(String,a)] -> Map.Map String [a]
fieldMap fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String, Type var)] -> Type var
recordOf fields = Record fields Nothing

listOf :: RawType -> RawType
listOf t = Data (Var.Raw "_List") [t]

tupleOf :: [RawType] -> RawType
tupleOf ts = Data (Var.Raw $ "_Tuple" ++ show (length ts)) ts

instance (Var.ToString var, Pretty var) => Pretty (Type var) where
  pretty tipe =
    case tipe of
      Lambda _ _ -> P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where
          t:ts = map prettyLambda (collectLambdas tipe)
          prettyLambda t = case t of
                             Lambda _ _ -> P.parens (pretty t)
                             _ -> pretty t

      Var x -> P.text x

      Data name [t] | Var.toString name == "_List" ->
          P.brackets (pretty t)

      Data name tipes
          | Help.isTuple (Var.toString name) ->
              P.parens . P.sep . P.punctuate P.comma $ map pretty tipes
          | otherwise ->
              P.hang (P.text (Var.toString name)) 2 (P.sep $ map prettyParens tipes)

      Record _ _ ->
          P.braces $ case flattenRecord tipe of
                       (fields, Nothing) -> prettyFields fields
                       (fields, Just x) ->
                           P.hang (P.text x <+> P.text "|") 4 (prettyFields fields)
          where
            prettyField (f,t) = P.text f <+> P.text ":" <+> pretty t
            prettyFields fields = commaSep (map prettyField fields)

      Aliased name _ -> pretty name

collectLambdas :: Type var -> [Type var]
collectLambdas tipe =
  case tipe of
    Lambda arg body -> arg : collectLambdas body
    _ -> [tipe]

prettyParens :: (Var.ToString var, Pretty var) => Type var -> Doc
prettyParens tipe = parensIf (needed tipe) (pretty tipe)
  where
    needed t =
      case t of
        Aliased _ t' -> needed t'
        Lambda _ _ -> True
        Data name [_] | Var.toString name == "_List" -> False
        Data _ [] -> False
        Data _ _ -> True
        _ -> False

flattenRecord :: Type var -> ( [(String, Type var)], Maybe String )
flattenRecord tipe =
    case tipe of
      Var x -> ([], Just x)

      Record fields Nothing -> (fields, Nothing)

      Record fields (Just ext) ->
          let (fields',ext') = flattenRecord ext
          in  (fields' ++ fields, ext')

      Aliased _ tipe' -> flattenRecord tipe'

      _ -> error "Trying to flatten ill-formed record."

instance Binary var => Binary (Type var) where
  put tipe =
      case tipe of
        Lambda t1 t2 ->
            putWord8 0 >> put t1 >> put t2
        Var x ->
            putWord8 1 >> put x
        Data ctor tipes ->
            putWord8 2 >> put ctor >> put tipes
        Record fs ext ->
            putWord8 3 >> put fs >> put ext
        Aliased var t ->
            putWord8 4 >> put var >> put t

  get = do
      n <- getWord8
      case n of
        0 -> Lambda <$> get <*> get
        1 -> Var <$> get
        2 -> Data <$> get <*> get
        3 -> Record <$> get <*> get
        4 -> Aliased <$> get <*> get
        _ -> error "Error reading a valid type from serialized string"
