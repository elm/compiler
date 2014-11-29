module Imports (Point, BooleanExpr(Not), true, false, adt, func) where

type Point = { x : Float, y : Float }

data BooleanExpr
    = T
    | F
    | Not BooleanExpr
    | And BooleanExpr BooleanExpr
    | Or  BooleanExpr BooleanExpr

true = T

false = F

adt : HiddenADT
adt = HiddenADT

func : HiddenADT -> Int
func adt = 42

data HiddenADT = HiddenADT