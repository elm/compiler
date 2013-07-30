{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Location where

import qualified Text.Parsec.Pos as Parsec
import Data.Data

data SrcPos = Pos { line :: Int, column :: Int }
    deriving (Eq, Ord, Data, Typeable)

data SrcSpan = Span SrcPos SrcPos | NoSpan
    deriving (Eq, Ord, Data, Typeable)

data Located e = L SrcSpan e
    deriving (Eq, Ord, Data, Typeable)


instance Show SrcPos where
    show (Pos r c) = "Line " ++ show r ++ ", Column " ++ show c

instance Show SrcSpan where
  show span = 
      case span of
        Span start end -> show start
        NoSpan -> ""

instance Show e => Show (Located e) where
  show (L _ e) = show e

none = L NoSpan

at start end = L (Span (Pos (Parsec.sourceLine start) (Parsec.sourceColumn start))
                       (Pos (Parsec.sourceLine end  ) (Parsec.sourceColumn end  )))

merge (L s1 _) (L s2 _) = L span
    where span = case (s1,s2) of
                   (Span start _, Span _ end) -> Span start end
                   (_, NoSpan) -> s1
                   (NoSpan, _) -> s2

sameAs (L s _) = L s