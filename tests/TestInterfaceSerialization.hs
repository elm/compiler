{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestInterfaceSerialization where

import Test.Framework
import Paths_Elm (version)
import Data.Version (showVersion)

import qualified InterfaceSerialization as IS
import SourceSyntax.Module
import qualified Data.Map as M


test_invalidVersion =
    assertBool $ isLeft
                   (IS.validVersion "foo" ("name", moduleInterface "0.0.0"))

test_validVersion =
    assertBool $ isRight
                   (IS.validVersion "foo" ("name", moduleInterface
                                                     (showVersion version)))

moduleInterface version =  ModuleInterface {
                             iVersion  = version
                           , iTypes    = M.fromList []
                           , iImports  = []
                           , iAdts     = []
                           , iAliases  = []
                           , iFixities = []
                           }

isLeft (Left _) = True
isLeft _        = False

isRight (Right _) = True
isRight _        = False
