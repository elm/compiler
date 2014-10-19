{- | This module exports functions for compiling Elm to JS.
-}
module Elm.Internal.Utils (compile, compileInOrder, moduleName, nameAndImports) where

import qualified Data.List as List
import qualified Generate.JavaScript as JS
import qualified Build.Source as Source
import qualified Parse.Module as Parser
import qualified AST.Module as M
import Parse.Parse (dependencies)
import qualified Text.PrettyPrint as P
import qualified Metadata.Prelude as Prelude
import System.IO.Unsafe
import qualified AST.Module as Module
import Data.Foldable (foldlM)
import Data.Map (insert)

-- |Helper function to check if a module compiled correctly, and either render it
--  or generate an error message 
canonicalToString :: Either [P.Doc] Module.CanonicalModule -> Either String String
canonicalToString canon = case canon of
      Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
      Right modul -> Right $ JS.generate modul

-- |This function compiles Elm code to JavaScript. It will return either
--  an error message or the compiled JS code.
compile :: String -> Either String String
compile source = canonicalToString $ Source.build False interfaces source

-- |Helper function for compileAll, used in a monadic foldL
-- Takes the last module compiled, and uses it in the interface of the next one, with possibility of failure.       
buildFun :: (String, Module.Interfaces, Module.CanonicalModule) -> (String, String) -> Either [P.Doc] (String, Module.Interfaces, Module.CanonicalModule)
buildFun (interName, oldInters, canon) (name, source) = bundleResult `fmap` eitherCanon
    where 
        bundleResult c  = (name, newInters, c)
        newInters = insert interName (Module.toInterface canon) oldInters 
        eitherCanon = Source.build False newInters source

-- |Helper function to get the module name of a source code string        
getName source = case Parser.getModuleName source of
                       Just n -> n
                       Nothing -> "Main"         
        
-- |Compile several modules into one javascript output, in the order given.
--  The last element of the list is assumed to be the main module
--  All modules should occur after their dependencies in the list
compileInOrder :: [String] -> Either String String
compileInOrder [] = Right ""
compileInOrder sources = canonicalToString $ do
    let ((firstName, firstSource) : otherNameSources) = map (\s -> (getName s,s)) sources
    firstCanonical <- Source.build False interfaces firstSource
    let firstBundle = (getName firstSource, interfaces, firstCanonical )
    (_, _, finalCanonical) <- foldlM buildFun firstBundle otherNameSources
    return finalCanonical
  
{-# NOINLINE interfaces #-}
interfaces :: M.Interfaces
interfaces = unsafePerformIO $ Prelude.interfaces False

-- |This function extracts the module name of a given source program.
moduleName :: String -> Maybe String
moduleName = Parser.getModuleName

-- |This function extracts the module name and imported modules from a given
--  source program.
nameAndImports :: String -> Maybe (String, [String])
nameAndImports src =
    either (const Nothing) Just (dependencies src)
