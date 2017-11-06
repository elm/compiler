{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment
  ( Env.Result
  , Env.Env
  , Env.addLocals
  , Env.findVar
  , Env.findType
  , Env.findPattern
  , Env.findBinop
  , Env.Binop(Env.Binop)
  , Env.Pattern(Env.Pattern)
  )
  where


import qualified Canonicalize.Environment.Internals as Env
