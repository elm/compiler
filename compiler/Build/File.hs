{-# OPTIONS_GHC -W #-}
module Build.File (build) where

import Control.Applicative      ((<$>))
import Control.Monad.Error      (runErrorT)
import Control.Monad.RWS.Strict
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Data.Binary            as Binary
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import qualified Data.Map               as Map
import qualified Data.ByteString.Lazy   as L

import qualified Build.Dependencies     as Deps
import qualified Build.Flags            as Flag
import qualified Build.Interface        as Interface
import qualified Build.Print            as Print
import qualified Build.Source           as Source
import qualified Build.SrcFile          as SrcFile
import Build.SrcFile (SrcFile)
import qualified Build.Utils            as Utils
import qualified Generate.JavaScript    as JS
import qualified Parse.Module           as Parser
import qualified AST.Module             as Module

-- Reader: Runtime flags, always accessible
-- Writer: Remember the last module to be accessed
-- State:  Build up a map of the module interfaces
type BuildT m a = RWST Flag.Flags (Last String) BInterfaces m a
type Build a = BuildT IO a

-- Interfaces, remembering if something was recompiled
type BInterfaces = Map.Map String (Bool, Module.Interface)

evalBuild :: Flag.Flags -> Module.Interfaces -> Build ()
          -> IO (Map.Map String Module.Interface, Maybe String)
evalBuild flags interfaces build =
  do (ifaces, moduleNames) <- execRWST build flags (fmap notUpdated interfaces)
     return (fmap snd ifaces, getLast moduleNames)
  where
    notUpdated iface = (False, iface)

-- | Builds a list of files, returning the moduleName of the last one.
--   Returns \"\" if the list is empty
build :: Flag.Flags -> Module.Interfaces -> [SrcFile] -> IO String
build flags interfaces files =
  do (ifaces, topName) <- evalBuild flags interfaces (buildAll files)
     let removeTopName = Maybe.maybe id Map.delete topName
     mapM_ (checkPorts topName) (Map.toList $ removeTopName ifaces)
     return $ Maybe.fromMaybe "" topName
  where
    checkPorts topName (name,iface)
        | null ports = return ()
        | otherwise  = Print.failure msg
        where
          ports = Module.iPorts iface
          msg = concat
            [ "Port Error: ports may only appear in the main module, but\n"
            , "    sub-module ", name, " declares the following port"
            , if length ports == 1 then "" else "s", ": "
            , List.intercalate ", " ports
            , case topName of
                Nothing -> ""
                Just tname -> "\n    All ports must appear in module " ++ tname
            ]

buildAll :: [SrcFile] -> Build ()
buildAll fs = mapM_ (uncurry build1) (zip [1..] fs)
  where build1 :: Integer -> SrcFile -> Build ()
        build1 num fname = do
          shouldCompile <- shouldBeCompiled fname
          if shouldCompile
            then compile number fname
            else retrieve fname

          where number = join ["[", show num, " of ", show total, "]"]

        total = length fs

shouldBeCompiled :: SrcFile -> Build Bool
shouldBeCompiled src = do
  flags <- ask
  let filePath = SrcFile.toPath src

      alreadyCompiled = liftIO $ do
        existsi <- doesFileExist (Utils.elmi flags src)
        existso <- doesFileExist (Utils.elmo flags src)
        return $ existsi && existso

      outDated = liftIO $ do
        tsrc <- getModificationTime $ filePath
        tint <- getModificationTime (Utils.elmo flags src)
        return (tsrc > tint)

      dependenciesUpdated = do
        eDeps <- liftIO . runErrorT $ Deps.readDeps filePath
        case eDeps of
          -- Should never actually reach here
          Left  err       -> liftIO $ Print.failure err
          Right (_, deps) -> anyM wasCompiled deps
        
  
    in (not <$> alreadyCompiled) `orM` outDated `orM` dependenciesUpdated

wasCompiled :: String -> Build Bool
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

retrieve :: SrcFile -> Build ()
retrieve src = do
  flags <- ask
  iface <- liftIO $ Interface.load (Utils.elmi flags src)
  case Interface.isValid (SrcFile.toPath src) iface of
    Right (name, interface) ->
      do liftIO $ when (Flag.print_types flags) (Print.types (Module.iTypes interface))
         update name interface False

    Left err -> liftIO $ Print.failure err

compile :: String -> SrcFile -> Build ()
compile number src =
  do flags      <- ask
     binterfaces <- get
     source <- liftIO $ readFile filePath
     let interfaces = snd <$> binterfaces
         name = getName source
     liftIO $ do
       printStatus name
       createDirectoryIfMissing True (Flag.cache_dir flags)
       createDirectoryIfMissing True (Flag.build_dir flags)

     canonicalModule <- 
       liftIO $ case Source.build (Flag.no_prelude flags) interfaces source of
           Right modul -> return modul
           Left errors -> do Print.errors errors
                             exitFailure

     liftIO $ when (Flag.print_types flags) $ do
       Print.types (Module.types (Module.body canonicalModule))
  
     let newInters = Module.toInterface canonicalModule
     generateCache name newInters canonicalModule
     update name newInters True

  where
    filePath = SrcFile.toPath src
    
    getName source = case Parser.getModuleName source of
                       Just n -> n
                       Nothing -> "Main"

    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name
                                  , replicate (max 1 (20 - length name)) ' '
                                  , "( " ++ filePath ++ " )" ]

    generateCache name interfs canonicalModule = do
      flags <- ask
      liftIO $ do
        createDirectoryIfMissing True . dropFileName $ Utils.elmi flags src
        writeFile (Utils.elmo flags src) (JS.generate canonicalModule)
        withBinaryFile (Utils.elmi flags src) WriteMode $ \handle ->
          L.hPut handle (Binary.encode (name, interfs))

update :: String -> Module.Interface -> Bool -> Build ()
update name inter wasUpdated =
  do modify (Map.insert name (wasUpdated, inter))
     tell (Last . Just $ name)
