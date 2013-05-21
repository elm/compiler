
module Context where

import Text.Parsec.Pos


data SrcPos = Pos Int Int
    deriving (Eq,Ord)

data SrcSpan = Span SrcPos SrcPos | NoSpan
    deriving (Eq,Ord)

data Context e = C (Maybe String) SrcSpan e deriving (Eq,Ord)


instance Show SrcPos where
    show (Pos r c) = "Line " ++ show r ++ ", Column " ++ show c

instance Show SrcSpan where
  show span = 
      case span of
        Span start end -> show start
        NoSpan -> ""

instance Show e => Show (Context e) where
  show (C _ _ e) = show e



noContext = C Nothing NoSpan

pos start end = C Nothing
                  (Span (Pos (sourceLine start) (sourceColumn start))
                        (Pos (sourceLine end  ) (sourceColumn end  )))

epos (C _ s1 _) (C _ s2 _) = C Nothing span
    where span = case (s1,s2) of
                   (Span start _, Span _ end) -> Span start end
                   (_, NoSpan) -> s1
                   (NoSpan, _) -> s2

addCtx x (C Nothing span e) = C (Just (show x)) span e
addCtx x (C txt span e) = C txt span e