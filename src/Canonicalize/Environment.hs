{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment
  ( Env.Result
  , Env.Env
  , create
  , Env.addLocals
  , Env.findVar
  , Env.findType
  , Env.findPattern
  , Env.findBinop
  , Env.Binop(Env.Binop)
  )
  where



import qualified Data.Map.Strict as Map

import qualified AST.Expression.Valid as Valid
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Environment.Foreign as Foreign
import qualified Canonicalize.Environment.Internals as Env
import qualified Canonicalize.Environment.Local as Local
import qualified Elm.Interface as I
import qualified Elm.Package as Package
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result a =
  Result.Result () Warning.Warning Error.Error a



-- CREATE


type ImportDict =
  Map.Map ModuleName.Raw ModuleName.Canonical


create :: Package.Name -> ImportDict -> I.Interfaces -> Valid.Module -> Result Env.Env
create pkg importDict interfaces module_@(Valid.Module name _ _ _ imports _ _ _ _ _) =
  let home = ModuleName.Canonical pkg name in
  Local.addDeclarations module_ =<<
    Foreign.createInitialEnv home importDict interfaces imports
