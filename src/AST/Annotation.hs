module AST.Annotation where

import qualified Text.Parsec.Pos as Parsec
import qualified Text.PrettyPrint as P
import AST.PrettyPrint

data Annotated annotation expr = A annotation expr
    deriving (Show)

data Region
    = Span Position Position P.Doc
    | None P.Doc
    deriving (Show)

data Position = Position
    { line :: Int
    , column :: Int
    } deriving (Show)

type Located expr = Annotated Region expr

none e = A (None (pretty e)) e
noneNoDocs e = A (None P.empty) e

at :: (Pretty expr) => Parsec.SourcePos -> Parsec.SourcePos -> expr
   -> Annotated Region expr
at start end e =
    A (Span (position start) (position end) (pretty e)) e
    where
      position loc = Position (Parsec.sourceLine loc) (Parsec.sourceColumn loc)

merge (A s1 _) (A s2 _) e =
    A (span (pretty e)) e
    where
      span = case (s1,s2) of
               (Span start _ _, Span _ end _) -> Span start end
               (Span start end _, _) -> Span start end
               (_, Span start end _) -> Span start end
               (_, _) -> None

mergeOldDocs (A s1 _) (A s2 _) e =
    A span e
    where
      span = case (s1,s2) of
               (Span start _ d1, Span _ end d2) ->
                   Span start end (P.vcat [d1, P.text "\n", d2])

               (Span _ _ _, _) -> s1
               (_, Span _ _ _) -> s2
               (_, _) -> None P.empty

sameAs :: Annotated a expr -> expr' -> Annotated a expr'
sameAs (A annotation _) expr = A annotation expr

getRegionDocs region =
    case region of
      Span _ _ doc -> doc
      None doc -> doc

instance Pretty Region where
  pretty span = 
      case span of
        None _ -> P.empty
        Span start end _ ->
            P.text $
            case line start == line end of
              False -> "between lines " ++ show (line start) ++ " and " ++ show (line end)
              True -> "on line " ++ show (line end) ++ ", column " ++
                      show (column start) ++ " to " ++ show (column end)

instance Pretty e => Pretty (Annotated a e) where
  pretty (A _ e) = pretty e

