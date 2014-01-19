module SourceSyntax.Location where

import Text.PrettyPrint
import SourceSyntax.PrettyPrint
import qualified Text.Parsec.Pos as Parsec

data SrcPos = Pos { line :: Int, column :: Int }
    deriving (Eq, Ord)

data SrcSpan = Span SrcPos SrcPos String | NoSpan String
    deriving (Eq, Ord)

data Located e = L SrcSpan e
    deriving (Eq, Ord)

none e = L (NoSpan (render $ pretty e)) e
noneNoDocs = L (NoSpan "")

at start end e = L (Span (Pos (Parsec.sourceLine start) (Parsec.sourceColumn start))
                         (Pos (Parsec.sourceLine end  ) (Parsec.sourceColumn end  ))
                         (render $ pretty e)) e

merge (L s1 _) (L s2 _) e = L (span (render $ pretty e)) e
    where span = case (s1,s2) of
                   (Span start _ _, Span _ end _) -> Span start end
                   (Span start end _, _) -> Span start end
                   (_, Span start end _) -> Span start end
                   (_, _) -> NoSpan

mergeOldDocs (L s1 _) (L s2 _) e = L span e
    where span = case (s1,s2) of
                   (Span start _ d1, Span _ end d2) -> Span start end (d1 ++ "\n\n" ++ d2)
                   (Span _ _ _, _) -> s1
                   (_, Span _ _ _) -> s2
                   (_, _) -> NoSpan ""

sameAs (L s _) = L s

instance Pretty SrcPos where
    pretty (Pos r c) = int r <> char ',' <> int c

instance Pretty SrcSpan where
  pretty span = case span of
    NoSpan _ -> empty
    Span start end _ ->
      case line start == line end of
        False -> text "between lines " <> int (line start) <> text " and " <> int (line end)
        True -> text "on line " <> int (line end) <> text ", column " <>
                int (column start) <> text " to " <> int (column end)

instance Pretty a => Pretty (Located a) where
  pretty (L _ e) = pretty e

