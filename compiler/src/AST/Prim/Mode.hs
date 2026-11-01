module AST.Prim.Mode
  ( Mode(..)
  , toPackage
  , isCore
  , isKernel
  )
  where


import qualified Elm.Package as Pkg



-- MODE


data Mode
  = Package Pkg.Name
  | Application


toPackage :: Mode -> Pkg.Name
toPackage mode =
  case mode of
    Package pkg -> pkg
    Application -> Pkg.dummyName


isCore :: Mode -> Bool
isCore projectType =
  case projectType of
    Package pkg -> pkg == Pkg.core
    Application -> False


isKernel :: Mode -> Bool
isKernel projectType =
  case projectType of
    Package pkg -> Pkg.isKernel pkg
    Application -> False

