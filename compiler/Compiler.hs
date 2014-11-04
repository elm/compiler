{-# OPTIONS_GHC -Wall #-}
module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)


main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      error "currently out of order"

{--
compileArgs :: Flag.Flags -> IO ()
compileArgs flags =
  do when (Flag.get_runtime flags) $ do
       putStrLn Utils.runtime
       exitSuccess
     case Flag.files flags of
       [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
       fs -> mapM_ (build flags) fs

build :: Flag.Flags -> FilePath -> IO ()
build flags rootFile =
    do let noPrelude = Flag.no_prelude flags
       builtIns <- Prelude.interfaces noPrelude

       (Recipe elmFiles jsFiles) <-
           if Flag.make flags
             then Utils.run (getBuildRecipe (Flag.src_dir flags) builtIns rootFile)
             else return (Recipe [rootFile] [])

       moduleName <- File.build flags builtIns elmFiles

       let elmos = map (Utils.elmo flags) elmFiles
       js <- foldM appendToOutput BS.empty (elmos ++ jsFiles)

       (extension, code) <-
           if Flag.only_js flags
           then do putStr "Generating JavaScript ... "
                   makeJs js
           else do putStr "Generating HTML ... "
                   makeHtml js moduleName

       let targetFile = Utils.buildPath flags rootFile extension
       createDirectoryIfMissing True (takeDirectory targetFile)
       BS.writeFile targetFile code
       putStrLn "Done"

    where
      appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
      appendToOutput js filePath = do
        src <- BS.readFile filePath
        return (BS.append src js)

      getRuntime :: IO Html.JSSource
      getRuntime =
          let runtimePath = Maybe.fromMaybe Utils.runtime (Flag.set_runtime flags) in
          case Flag.bundle_runtime flags of
            False -> return (Html.Link runtimePath)
            True  -> Html.Source `fmap` BS.readFile runtimePath

      makeJs :: BS.ByteString -> IO (String, BS.ByteString)
      makeJs js =
        do runtime <- getRuntime
           case runtime of
             Html.Link _ -> return ("js", js)
             Html.Source rts -> return ("js", BS.append rts js)

      makeHtml :: BS.ByteString -> String -> IO (String, BS.ByteString)
      makeHtml js moduleName =
        do runtime <- getRuntime
           return ("html", BS.pack $ renderHtml (html runtime))
        where
          sources js = map Html.Link (Flag.scripts flags) ++ [ Html.Source js ]
          html runtime =
              Html.generate runtime (takeBaseName rootFile) (sources js) moduleName ""
--}
