{-# LANGUAGE DeriveDataTypeable #-}
module Located where

import Text.Parsec.Pos
import Data.Data

data SrcPos = Pos Int Int
    deriving (Eq, Ord, Data, Typeable)

data SrcSpan = Span SrcPos SrcPos | NoSpan
    deriving (Eq, Ord, Data, Typeable)

data Located e = L (Maybe String) SrcSpan e deriving (Eq,Ord, Data, Typeable)


instance Show SrcPos where
    show (Pos r c) = "Line " ++ show r ++ ", Column " ++ show c

instance Show SrcSpan where
  show span = 
      case span of
        Span start end -> show start
        NoSpan -> ""

instance Show e => Show (Located e) where
  show (L _ _ e) = show e



notLocated = L Nothing NoSpan

pos start end = L Nothing
                  (Span (Pos (sourceLine start) (sourceColumn start))
                        (Pos (sourceLine end  ) (sourceColumn end  )))

epos (L _ s1 _) (L _ s2 _) = L Nothing span
    where span = case (s1,s2) of
                   (Span start _, Span _ end) -> Span start end
                   (_, NoSpan) -> s1
                   (NoSpan, _) -> s2

addLoc x (L Nothing span e) = L (Just (show x)) span e
addLoc x (L txt span e) = L txt span e