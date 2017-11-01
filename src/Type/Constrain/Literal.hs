{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Literal (constrain) where

import qualified AST.Literal as Lit
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Type.Constraint as T
import qualified Type.Type as T



-- CONSTRAIN LITERALS


constrain :: R.Region -> Lit.Literal -> T.Type -> IO T.Constraint
constrain region literal tipe =
  case literal of
    Lit.IntNum _ ->
      do  var <- T.mkFlexNumber
          return $ T.CEqual numberError region (T.VarN var) tipe

    Lit.FloatNum _ ->
      return $ T.CEqual floatError region T.float tipe

    Lit.Chr _ ->
      return $ T.CEqual charError region T.char tipe

    Lit.Str _ ->
      return $ T.CEqual stringError region T.string tipe



-- LITERAL ERRORS


{-# NOINLINE numberError #-}
numberError :: Error.Hint
numberError =
  Error.Literal "number"


{-# NOINLINE floatError #-}
floatError :: Error.Hint
floatError =
  Error.Literal "float"


{-# NOINLINE charError #-}
charError :: Error.Hint
charError =
  Error.Literal "character"


{-# NOINLINE stringError #-}
stringError :: Error.Hint
stringError =
  Error.Literal "string"
