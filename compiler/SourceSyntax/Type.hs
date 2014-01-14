{-# OPTIONS_GHC -W #-}
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
          | Record [(String,Type)] (Maybe String)
            deriving (Eq, Show)

fieldMap :: [(String,a)] -> Map.Map String [a]
fieldMap fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = Record fields Nothing

listOf :: Type -> Type
listOf t = Data "_List" [t]

tupleOf :: [Type] -> Type
tupleOf ts = Data ("_Tuple" ++ show (length ts)) ts


instance Pretty Type where
  pretty tipe =
    case tipe of
      Lambda _ _ -> P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where
          t:ts = map prettyLambda (collectLambdas tipe)
          prettyLambda t = case t of
                             Lambda _ _ -> P.parens (pretty t)
                             _ -> pretty t

      Var x -> P.text x
      Data "_List" [t] -> P.brackets (pretty t)
      Data name tipes
          | Help.isTuple name -> P.parens . P.sep . P.punctuate P.comma $ map pretty tipes
          | otherwise -> P.hang (P.text name) 2 (P.sep $ map prettyParens tipes)
      Record fields ext ->
          P.braces $ case ext of
                       Nothing -> prettyFields
                       Just x -> P.hang (P.text x <+> P.text "|") 4 prettyFields
          where
            prettyField (f,t) = P.text f <+> P.text ":" <+> pretty t
            prettyFields = commaSep . map prettyField $ fields

collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body -> arg : collectLambdas body
    _ -> [tipe]

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
        Record fs ext ->
            putWord8 3 >> put fs >> put ext

  get = do
      n <- getWord8
      case n of
        0 -> Lambda <$> get <*> get
        1 -> Var <$> get
        2 -> Data <$> get <*> get
        3 -> Record <$> get <*> get
        _ -> error "Error reading a valid type from serialized string"
