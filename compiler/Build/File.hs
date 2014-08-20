{-# OPTIONS_GHC -W #-}
module Build.File (build) where

import Control.Applicative ((<$>))
import Control.Monad.RWS.Strict (Last(Last), RWST, execRWST, ask, get, getLast,
                                 liftIO, modify, tell, when)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.Exit (exitFailure)
import System.FilePath (dropFileName, takeDirectory)
import System.IO (IOMode(WriteMode), hPutStrLn, stdout, withBinaryFile)

import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L

import AST.Helpers (splitDots)
import qualified AST.Annotation as A
import qualified AST.Module as Module
import qualified Elm.Internal.Documentation as Doc
import qualified Build.Flags as Flag
import qualified Build.Interface as Interface
import qualified Build.Metadata as Metadata
import qualified Build.Print as Print
import qualified Build.Source as Source
import qualified Build.Utils as Utils
import qualified Generate.JavaScript as JS

-- Reader: Runtime flags, always accessible
-- Writer: Remember the last module to be accessed
-- State:  Build up a map of the module interfaces
type BuildT m a =
    RWST Flag.Flags (Last Metadata.WithDeps) BuiltInterfaces m a

type Build a = BuildT IO a

-- Interfaces, remembering if something was recompiled
type BuiltInterfaces =
    Map.Map Metadata.WithDeps (Bool, Module.Interface)

runBuild :: Flag.Flags -> Module.Interfaces -> Build ()
          -> IO (Map.Map Metadata.WithDeps Module.Interface, Maybe Metadata.WithDeps)
runBuild flags builtins build =
  do (interfaces, moduleNames) <-
         execRWST build flags builtInterfaces
     return ( Map.map snd interfaces
            , getLast moduleNames
            )
  where
    builtInterfaces =
         Map.map ((,) False) (Map.mapKeys dummyPath builtins)

    dummyPath name =
        Metadata.Metadata "" (splitDots name) Nothing []

-- | Builds a list of files, returning the moduleName of the last one.
--   Returns \"\" if the list is empty
build :: Flag.Flags -> Module.Interfaces -> [Metadata.WithDeps] -> IO String
build flags builtins paths =
  do (ifaces, rootPath) <- runBuild flags builtins (createBuild paths)

     -- Check to see that no ports appear outside of the root module.
     let removeRoot = Maybe.maybe id Map.delete rootPath
     mapM_ (checkPorts rootPath) (Map.toList (removeRoot ifaces))

     -- TODO: maybe return a Maybe or a Module.Name?
     return (Maybe.maybe "" Metadata.moduleName rootPath)
  where
    checkPorts rootPath (metadata, iface)
        | null ports = return ()
        | otherwise  = Print.failure msg
        where
          ports = Module.iPorts iface
          name  = Metadata.moduleName metadata
          msg = concat
            [ "Port Error: ports may only appear in the main module, but\n"
            , "    sub-module ", name, " declares the following port"
            , if length ports == 1 then "" else "s", ": "
            , List.intercalate ", " ports
            , case rootPath of
                Nothing -> ""
                Just mdata ->
                    "\n    All ports must appear in module " ++ Metadata.moduleName mdata
            ]

createBuild :: [Metadata.WithDeps] -> Build ()
createBuild paths =
    mapM_ build1 (zip [1..] paths)
  where
    progress num =
        "[" ++ show num ++ " of " ++ show (length paths) ++ "]"

    build1 :: (Int, Metadata.WithDeps) -> Build ()
    build1 (num, path) =
      do shouldCompile <- shouldBeCompiled path
         if shouldCompile
           then compile (progress num) path
           else retrieve path

shouldBeCompiled :: Metadata.WithDeps -> Build Bool
shouldBeCompiled path =
  do flags <- ask
     let elmi = Utils.elmi flags path
         elmo = Utils.elmo flags path
     (not <$> alreadyCompiled elmi elmo) `orM` outdated elmo `orM` dependenciesUpdated

  where
    alreadyCompiled elmi elmo =
        liftIO $ do
          existsi <- doesFileExist elmi
          existso <- doesFileExist elmo
          return (existsi && existso)

    outdated elmo =
        liftIO $ do
          tsrc <- getModificationTime (Metadata._path path)
          tint <- getModificationTime elmo
          return (tsrc > tint)

    dependenciesUpdated =
        do builtInterfaces <- Map.toList <$> get
           return $ any (wasUpdated builtInterfaces) (Metadata._deps path)
        where
          wasUpdated builtInterfaces location =
              any (matchingUpdate (Metadata._name location)) builtInterfaces 

          matchingUpdate name (path, (updated, _interface)) =
              Metadata._name path == name && updated
  
-- Short-circuiting monadic (||)
infixr 2 `orM`
orM :: (Monad m) => m Bool -> m Bool -> m Bool
orM check1 check2 =
  do result <- check1
     if result then return True else check2

retrieve :: Metadata.WithDeps -> Build ()
retrieve path =
  do flags <- ask
     iface <- liftIO $ Interface.load (Utils.elmi flags path)
     case Interface.isValid (Metadata._path path) iface of
       Left err ->
           liftIO $ Print.failure err

       Right (_, interface) ->
           do liftIO $ when (Flag.print_types flags) (Print.types (Module.iTypes interface))
              addBuiltInterface path interface False

compile :: String -> Metadata.WithDeps -> Build ()
compile number path =
  do flags <- ask
     binterfaces <- get
     source <- liftIO $ readFile (Metadata._path path)
     let interfaces = toInterfaces binterfaces
         name = Metadata.moduleName path
     liftIO $ do
       printStatus name
       createDirectoryIfMissing True (Flag.cache_dir flags)
       createDirectoryIfMissing True (Flag.build_dir flags)

     canonicalModule <- 
       liftIO $ do
         case Source.build (Flag.no_prelude flags) interfaces source of
           Right modul -> return modul
           Left errors -> do Print.errors errors
                             exitFailure

     let documentation = Doc.generateDocumentation canonicalModule
     liftIO $ do let docPath = Utils.docsPath path
                 createDirectoryIfMissing True (takeDirectory docPath)
                 L.writeFile docPath documentation

     liftIO $ when (Flag.print_types flags) $ do
       Print.types (Map.map A.value (Module.types (Module.body canonicalModule)))
  
     let newInterface = Module.toInterface canonicalModule
     generateCache name newInterface canonicalModule
     addBuiltInterface path newInterface True

  where
    toInterfaces :: BuiltInterfaces -> Module.Interfaces
    toInterfaces builtInterfaces =
        Map.mapKeys Metadata.moduleName (Map.map snd builtInterfaces)
    
    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name ]

    generateCache name interfs canonicalModule = do
      flags <- ask
      let elmi = Utils.elmi flags path
          elmo = Utils.elmo flags path
      liftIO $ do
        createDirectoryIfMissing True . dropFileName $ elmi
        writeFile elmo (JS.generate canonicalModule)
        withBinaryFile elmi WriteMode $ \handle ->
          L.hPut handle (Binary.encode (name, interfs))

addBuiltInterface :: Metadata.WithDeps -> Module.Interface -> Bool -> Build ()
addBuiltInterface name interface wasUpdated =
  do modify (Map.insert name (wasUpdated, interface))
     tell (Last (Just name))
