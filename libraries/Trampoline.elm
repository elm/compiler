module Trampoline (Trampoline, trampoline)
       where

{-|  -}
import Native.Trampoline
import open Either

data Trampoline a = Trampoline (Either a (() -> Trampoline a))

trampoline : Trampoline a -> a
trampoline = Native.Trampoline.trampoline
