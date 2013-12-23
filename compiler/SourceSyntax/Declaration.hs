module SourceSyntax.Declaration where

import qualified SourceSyntax.Expression as Expr
import SourceSyntax.Type
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Declaration tipe var
    = Definition (Expr.Def tipe var)
    | Datatype String [String] [(String,[Type])]
    | TypeAlias String [String] Type [Deriveable]
    | ImportEvent String (Expr.LExpr tipe var) String Type
    | ExportEvent String String Type
    | Fixity Assoc Int String
      deriving (Eq, Show)

data Assoc = L | N | R
    deriving (Eq)

data Deriveable = Json | Binary | RecordConstructor
    deriving (Eq, Show)

instance Show Assoc where
    show assoc =
        case assoc of
          L -> "left"
          N -> "non"
          R -> "right"

instance Pretty (Declaration t v) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors ->
          P.hang (P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)) 4
               (P.sep $ zipWith join ("=" : repeat "|") ctors)
          where
            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map prettyParens tipes))

      TypeAlias name tvars tipe deriveables ->
          P.hang (P.text "type" <+> alias <+> P.equals) 4 (pretty tipe) <+> deriving'
          where
            alias = P.text name <+> P.hsep (map P.text tvars)
            deriving' =
                case deriveables of
                  []  -> P.empty
                  [d] -> P.text "deriving" <+> P.text (show d)
                  ds  -> P.text "deriving" <+>
                         P.parens (P.hsep $ P.punctuate P.comma $ map (P.text . show) ds)

      -- TODO: Actually write out the contained data in these cases.
      ImportEvent _ _ _ _ -> P.text (show decl)
      ExportEvent _ _ _   -> P.text (show decl)
      Fixity _ _ _        -> P.text (show decl)
