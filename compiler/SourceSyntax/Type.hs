{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Type where

import Data.Binary
import qualified Data.Map as Map
import qualified SourceSyntax.Helpers as Help
import Control.Applicative ((<$>), (<*>))
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Type = Lambda Type Type
          | Var String
          | Data String [Type]
          | EmptyRecord
          | Record [(String,Type)] Type
            deriving (Eq, Show)

fieldMap :: [(String,a)] -> Map.Map String [a]
fieldMap fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = Record fields EmptyRecord

listOf :: Type -> Type
listOf t = Data "_List" [t]

tupleOf :: [Type] -> Type
tupleOf ts = Data ("_Tuple" ++ show (length ts)) ts


instance Pretty Type where
  pretty tipe =
    case tipe of
      Lambda _ _ -> P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where t:ts = collectLambdas tipe
      Var x -> P.text x
      Data "_List" [t] -> P.brackets (pretty t)
      Data name tipes
          | Help.isTuple name -> P.parens . P.sep . P.punctuate P.comma $ map pretty tipes
          | otherwise -> P.hang (P.text name) 2 (P.sep $ map prettyParens tipes)
      EmptyRecord -> P.braces P.empty
      Record _ _ -> P.braces $ case ext of
                                 EmptyRecord -> prettyFields
                                 _ -> P.hang (pretty ext <+> P.text "|") 4 prettyFields
          where
            (fields, ext) = collectRecords tipe
            prettyField (f,t) = P.text f <+> P.text ":" <+> pretty t
            prettyFields = commaSep . map prettyField $ fields

collectLambdas :: Type -> [Doc]
collectLambdas tipe =
  case tipe of
    Lambda arg@(Lambda _ _) body -> P.parens (pretty arg) : collectLambdas body
    Lambda arg body -> pretty arg : collectLambdas body
    _ -> [pretty tipe]

collectRecords :: Type -> ([(String,Type)], Type)
collectRecords = go []
  where
    go fields tipe =
        case tipe of
          Record fs ext -> go (fs ++ fields) ext
          _ -> (fields, tipe)

prettyParens :: Type -> Doc
prettyParens tipe = parensIf needed (pretty tipe)
  where
    needed =
      case tipe of
        Lambda _ _ -> True
        Data "_List" [_] -> False
        Data _ [] -> False
        Data _ _ -> True
        _ -> False

instance Binary Type where
  put tipe =
      case tipe of
        Lambda t1 t2 ->
            putWord8 0 >> put t1 >> put t2
        Var x ->
            putWord8 1 >> put x
        Data ctor tipes ->
            putWord8 2 >> put ctor >> put tipes
        EmptyRecord ->
            putWord8 3
        Record fs ext ->
            putWord8 4 >> put fs >> put ext

  get = do
      n <- getWord8
      case n of
        0 -> Lambda <$> get <*> get
        1 -> Var <$> get
        2 -> Data <$> get <*> get
        3 -> return EmptyRecord
        4 -> Record <$> get <*> get
        _ -> error "Error reading a valid type from serialized string"
