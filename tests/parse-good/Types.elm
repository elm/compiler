module Types exposing (..)

type alias Name = String

type Maybe a
  = Just a
  | Nothing

type Result error value
  = Ok value
  | Err error

type Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
