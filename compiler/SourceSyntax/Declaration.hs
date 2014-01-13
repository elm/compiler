{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Declaration where

import Data.Binary
import qualified SourceSyntax.Expression as Expr
import qualified SourceSyntax.Type as T
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Declaration' port def
    = Definition def
    | Datatype String [String] [(String,[T.Type])] [Derivation]
    | TypeAlias String [String] T.Type [Derivation]
    | Port port
    | Fixity Assoc Int String
      deriving (Show)

data Assoc = L | N | R
    deriving (Eq)

data Derivation = Json | JS | Binary | New
    deriving (Eq, Show)

data ParsePort
    = PPAnnotation String T.Type
    | PPDef String Expr.LParseExpr
      deriving (Show)

data Port
    = Out String Expr.LExpr T.Type
    | In String T.Type
      deriving (Show)

type ParseDeclaration = Declaration' ParsePort Expr.ParseDef
type Declaration = Declaration' Port Expr.Def

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

instance Binary Assoc where
    get = do n <- getWord8
             return $ case n of
                0 -> L
                1 -> N
                2 -> R
                _ -> error "Error reading valid associativity from serialized string"

    put assoc = putWord8 $ case assoc of { L -> 0 ; N -> 1 ; R -> 2 }

instance (Pretty port, Pretty def) => Pretty (Declaration' port def) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors deriveables ->
          P.hang (P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)) 4
               (P.sep $ zipWith join ("=" : repeat "|") ctors) <+> prettyDeriving deriveables
          where
            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map T.prettyParens tipes))

      TypeAlias name tvars tipe deriveables ->
          alias <+> prettyDeriving deriveables
          where
            name' = P.text name <+> P.hsep (map P.text tvars)
            alias = P.hang (P.text "type" <+> name' <+> P.equals) 4 (pretty tipe)

      Port port -> pretty port

      Fixity assoc prec op -> P.text "infix" <> assoc' <+> P.int prec <+> P.text op
          where
            assoc' = case assoc of
                       L -> P.text "l"
                       N -> P.empty
                       R -> P.text "r"

instance Pretty ParsePort where
  pretty port =
    case port of
      PPAnnotation name tipe -> prettyPort name ":"  tipe
      PPDef        name expr -> prettyPort name "=" expr

instance Pretty Port where
  pretty port =
    case port of
      In name tipe -> prettyPort name ":" tipe
      Out name expr tipe -> P.vcat [ prettyPort name ":" tipe
                                   , prettyPort name "=" expr ]
          

prettyPort :: (Pretty a) => String -> String -> a -> Doc
prettyPort name op e = P.text "port" <+> P.text name <+> P.text op <+> pretty e

prettyDeriving :: [Derivation] -> Doc
prettyDeriving deriveables =
    case deriveables of
      []  -> P.empty
      [d] -> P.text "deriving" <+> P.text (show d)
      ds  -> P.text "deriving" <+>
             P.parens (P.hsep $ P.punctuate P.comma $ map (P.text . show) ds)