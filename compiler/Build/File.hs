{-# OPTIONS_GHC -W #-}
module Build.File (build) where

import Control.Applicative ((<$>))
import Control.Monad.RWS.Strict
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L

import AST.Helpers (splitDots)
import qualified AST.Annotation as A
import qualified AST.Module as Module
import qualified AST.ProgramHeader as PH
import qualified Elm.Internal.Documentation as Doc
import qualified Build.Dependencies as Deps
import qualified Build.Flags as Flag
import qualified Build.Interface as Interface
import qualified Build.Print as Print
import qualified Build.Source as Source
import qualified Build.SrcFile as SrcFile
import qualified Build.Utils as Utils
import qualified Generate.JavaScript as JS

-- Reader: Runtime flags, always accessible
-- Writer: Remember the last module to be accessed
-- State:  Build up a map of the module interfaces
type BuildT m a = RWST Flag.Flags (Last PH.ModuleId) BInterfaces m a
type Build a = BuildT IO a

-- Interfaces, remembering if something was recompiled
type BInterfaces = Map.Map PH.ModuleId (Bool, Module.Interface)

evalBuild :: Flag.Flags -> Module.Interfaces -> Build ()
          -> IO (Map.Map PH.ModuleId Module.Interface, Maybe PH.ModuleId)
evalBuild flags builtins build =
  do (ifaces, moduleNames) <- execRWST build flags (toBInterface builtins)
     return (fmap snd ifaces, getLast moduleNames)
  where
    toBInterface     = fmap notUpdated . Map.mapKeys builtInModuleId
    builtInModuleId name = (Nothing, splitDots name)
    notUpdated iface = (False, iface)

-- | Builds a list of files, returning the moduleName of the last one.
--   Returns \"\" if the list is empty
build :: Flag.Flags -> Module.Interfaces -> [SrcFile.Resolved] -> IO String
build flags builtins files =
  do (ifaces, topName) <- evalBuild flags builtins (buildAll files)
     let removeTopName = Maybe.maybe id Map.delete topName
     mapM_ (checkPorts topName) (Map.toList $ removeTopName ifaces)
     return $ Maybe.maybe "" (PH.moduleName . snd) topName
  where
    checkPorts topName ((_, names),iface)
        | null ports = return ()
        | otherwise  = Print.failure msg
        where
          ports = Module.iPorts iface
          name  = PH.moduleName names
          msg = concat
            [ "Port Error: ports may only appear in the main module, but\n"
            , "    sub-module ", name, " declares the following port"
            , if length ports == 1 then "" else "s", ": "
            , List.intercalate ", " ports
            , case topName of
                Nothing -> ""
                Just (_, tname) -> "\n    All ports must appear in module " ++ PH.moduleName tname
            ]

buildAll :: [SrcFile.Resolved] -> Build ()
buildAll fs = mapM_ (uncurry build1) (zip [1..] fs)
  where build1 :: Integer -> SrcFile.Resolved -> Build ()
        build1 num srcFile = do
          shouldCompile <- shouldBeCompiled srcFile
          if shouldCompile
            then compile number srcFile
            else retrieve srcFile

          where number = join ["[", show num, " of ", show total, "]"]

        total = length fs

shouldBeCompiled :: SrcFile.Resolved -> Build Bool
shouldBeCompiled srcFile = do
  flags <- ask
  let elmi = Utils.elmi flags srcFile
      elmo = Utils.elmo flags srcFile

      alreadyCompiled = liftIO $ do
        existsi <- doesFileExist elmi
        existso <- doesFileExist elmo
        return $ existsi && existso

      outDated = liftIO $ do
        tsrc <- getModificationTime (SrcFile.path srcFile)
        tint <- getModificationTime elmo
        return (tsrc > tint)

      dependenciesUpdated = anyM wasCompiled (SrcFile.resolvedDeps srcFile)

    in (not <$> alreadyCompiled) `orM` outDated `orM` dependenciesUpdated

wasCompiled :: PH.ModuleId -> Build Bool
wasCompiled modul = maybe False fst . Map.lookup modul <$> get
  
-- Short-circuiting monadic (||)
infixr 2 `orM`
orM :: (Monad m) => m Bool -> m Bool -> m Bool
orM m1 m2 = do b1 <- m1
               if b1
                 then return b1
                 else m2

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM f = foldr (orM . f) (return False)

retrieve :: SrcFile.Resolved -> Build ()
retrieve srcFile = do
  flags <- ask
  iface <- liftIO $ Interface.load (Utils.elmi flags srcFile)
  case Interface.isValid (SrcFile.path srcFile) iface of
    Right (_, interface) ->
      do liftIO $ when (Flag.print_types flags) (Print.types (Module.iTypes interface))
         update mId interface False

    Left err -> liftIO $ Print.failure err

  where mId = SrcFile.moduleId srcFile

compile :: String -> SrcFile.Resolved -> Build ()
compile number srcFile =
  do flags      <- ask
     binterfaces <- get
     source <- liftIO $ readFile filePath
     let interfaces = toInterfaces binterfaces
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
     liftIO $ do let docPath = "docs" </> replaceExtension filePath ".json"
                 createDirectoryIfMissing True (takeDirectory docPath)
                 L.writeFile docPath documentation

     liftIO $ when (Flag.print_types flags) $ do
       Print.types (Map.map A.value (Module.types (Module.body canonicalModule)))
  
     let newInters = Module.toInterface canonicalModule
     generateCache name newInters canonicalModule
     update (SrcFile.moduleId srcFile) newInters True

  where
    name     = PH.moduleName . SrcFile.moduleName $ srcFile
    filePath = SrcFile.path srcFile

    toInterfaces :: BInterfaces -> Module.Interfaces
    toInterfaces = Map.mapKeys (PH.moduleName . snd) . fmap snd
    
    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name
                                  , replicate (max 1 (20 - length name)) ' '
                                  , "( " ++ filePath ++ " )" ]

    generateCache name interfs canonicalModule = do
      flags <- ask
      let elmi = Utils.elmi flags srcFile
          elmo = Utils.elmo flags srcFile
      liftIO $ do
        createDirectoryIfMissing True . dropFileName $ elmi
        writeFile elmo (JS.generate canonicalModule)
        withBinaryFile elmi WriteMode $ \handle ->
          L.hPut handle (Binary.encode (name, interfs))

update :: PH.ModuleId -> Module.Interface -> Bool -> Build ()
update name inter wasUpdated =
  do modify (Map.insert name (wasUpdated, inter))
     tell (Last . Just $ name)
