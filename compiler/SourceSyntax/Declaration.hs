{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Declaration where

import Data.Binary
import qualified SourceSyntax.Expression as Expr
import SourceSyntax.Type
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Declaration tipe var
    = Definition (Expr.Def tipe var)
    | Datatype String [String] [(String,[Type])] [Derivation]
    | TypeAlias String [String] Type [Derivation]
    | Port String Type (Maybe (Expr.LExpr tipe var))
    | Fixity Assoc Int String
      deriving (Eq, Show)

data Assoc = L | N | R
    deriving (Eq)

data Derivation = Json | JS | Binary | New
    deriving (Eq, Show)

instance Binary Derivation where
  get = do n <- getWord8
           return $ case n of
             0 -> Json
             1 -> JS
             2 -> Binary
             3 -> New
             _ -> error "Unable to decode Derivation. You may have corrupted binary files,\n\
                        \so please report an issue at <https://github.com/evancz/Elm/issues>"

  put derivation =
      putWord8 $ case derivation of
                   Json   -> 0
                   JS     -> 1
                   Binary -> 2
                   New    -> 3

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

      Datatype tipe tvars ctors deriveables ->
          P.hang (P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)) 4
               (P.sep $ zipWith join ("=" : repeat "|") ctors) <+> prettyDeriving deriveables
          where
            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map prettyParens tipes))

      TypeAlias name tvars tipe deriveables ->
          alias <+> prettyDeriving deriveables
          where
            name' = P.text name <+> P.hsep (map P.text tvars)
            alias = P.hang (P.text "type" <+> name' <+> P.equals) 4 (pretty tipe)

      Port name tipe maybeExpr ->
          let port = P.text "port" <+> P.text name in
          P.vcat [ port <+> P.colon  <+> pretty tipe
                 , maybe P.empty (\expr -> port <+> P.equals <+> pretty expr) maybeExpr
                 ]

      Fixity assoc prec op -> P.text "infix" <> assoc' <+> P.int prec <+> P.text op
          where
            assoc' = case assoc of
                       L -> P.text "l"
                       N -> P.empty
                       R -> P.text "r"

prettyDeriving :: [Derivation] -> Doc
prettyDeriving deriveables =
    case deriveables of
      []  -> P.empty
      [d] -> P.text "deriving" <+> P.text (show d)
      ds  -> P.text "deriving" <+>
             P.parens (P.hsep $ P.punctuate P.comma $ map (P.text . show) ds)
