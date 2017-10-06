{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Internals
  ( Error(..)
  , Warning(..)
  )
  where


import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Warning as Warning



-- OPAQUE TYPES


newtype Error = Error (A.Located Error.Error)
newtype Warning = Warning (A.Located Warning.Warning)

