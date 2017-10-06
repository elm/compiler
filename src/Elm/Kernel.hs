{-# OPTIONS_GHC -Wall #-}
module Elm.Kernel
  ( Kernel.Data(..)
  , Kernel.Content
  , parse
  )
  where


import Data.Text (Text)

import qualified AST.Kernel as Kernel (Data(..), Content(..))
import qualified Elm.Compiler.Internals as I
import qualified Parse.Helpers as Parse (run)
import qualified Parse.Kernel as Kernel (parser)
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error



-- PARSE KERNEL


parse :: Text -> Either I.Error Kernel.Content
parse sourceCode =
  case Parse.run Kernel.parser sourceCode of
    Right content ->
      Right content

    Left err ->
      Left (I.Error (A.map Error.Syntax err))
