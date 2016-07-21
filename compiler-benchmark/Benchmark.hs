module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import qualified Text.Tabular.AsciiArt as Table

import Control.Monad (unless, void, when)
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable (find)
import Data.IntMap ()
import Data.Map (Map)
import Data.Text as Text (Text, pack, unpack)
import GHC.RTS.TimeAllocProfile (timeAllocProfile) 
import GHC.RTS.TimeAllocProfile.Types
    ( TimeAllocProfile, profileCostCentreTree
    , CostCentre, costCentreNodes, costCentreName, costCentreIndTime
    )
import System.Directory 
    ( copyFile, doesFileExist
    , getCurrentDirectory, createDirectoryIfMissing, doesDirectoryExist
    , getDirectoryContents, removeDirectoryRecursive, setCurrentDirectory
    )
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (rawSystem)
import Text.Tabular 
    ( Table(Table), Header(Header, Group)
    , Properties(NoLine, SingleLine, DoubleLine)
    )

-- Represents a git repo that can be cloned, such as a publicly accessible
-- project written in Elm, or a component of the compiler.
data Repo = Repo
    { projectName :: String
    , path :: FilePath
    , branch :: String
    }

-- Version of the Elm compiler that is being used.
data Version
    = Gold 
    | Dev
    deriving Show

-- CONFIGURATION

-- The components of the Elm Platform to be benchmarked, along with the
-- branches to use as the baseline for each component. The branches
-- indicated here are compared to the current state of the enclosing
-- elm-compiler repository.
sources :: [Repo]
sources = 
    [ Repo 
        { projectName = "elm-make"
        , path = "https://github.com/elm-lang/elm-make.git"
        , branch = "master"
        }
    , Repo
        { projectName = "elm-package"
        , path = "https://github.com/elm-lang/elm-package.git"
        , branch = "master"
        }
    , Repo
        { projectName = "elm-compiler"
        , path = "https://github.com/elm-lang/elm-compiler.git"
        , branch = "master" 
        }
    ]   

-- Projects (written in Elm) to be compiled for benchmarking.
-- These projects should either be (a) representative of typical real-world Elm
-- programs (b) pathological programs known to test performance edge-cases in
-- the compiler.
projects :: [Repo]
projects = 
    [ Repo
        { projectName = "elm-todomvc"
        , path = "https://github.com/evancz/elm-todomvc.git"
        , branch = "master"
        }
    ]

-- Names of annotated cost centres in elm-compiler and elm-make. Must be kept
-- in sync with the SCC annotations in elm-compiler and elm-make (which must
-- also be kept in sync with each other). Used to extract cost centre data
-- from the results of profiling the compiler execution. 
costCentreNames :: [Text]
costCentreNames = map Text.pack
    [ "parsing"
    , "completeness"
    ]

main :: IO ()
main = do
    root <- getCurrentDirectory
    let benchmarkRoot = root </> "compiler-benchmark"
    inDirectory benchmarkRoot $ do
        -- Elm projects
        createDirectoryIfMissing True "benchmark-projects"
        inDirectory "benchmark-projects" $ mapM_ makeRepo projects
            
        -- Source 
        createDirectoryIfMissing True "benchmark-src"
        inDirectory "benchmark-src" $ do
            mapM_ (makeSrcRepo benchmarkRoot) sources

            -- Need to do additional setup with elm-make to ensure that we're
            -- building it with the local versions of elm-compiler and
            -- elm-package.
            let srcRoot = benchmarkRoot </> "benchmark-src"
            inDirectory "elm-make" $ do
                addSources
                    srcRoot
                    (filter (\repo -> projectName repo /= "elm-make") sources)

        -- Results
        createDirectoryIfMissing True "benchmark-results"

        -- Compile each project with the golden versions of elm-compiler, elm-make, etc.
        projects <- listDirectory "benchmark-projects"

        hasGoldResults <- doesFileExist 
            ("benchmark-results" </> "elm-make.Gold.prof")
        unless hasGoldResults $ mapM_ (compile Gold) projects

        -- Set the parent elm-compiler repo to be compiled into elm-make for
        -- development.
        inDirectory ("benchmark-src" </> "elm-make") $ do
            void . cabal $ ["sandbox", "add-source", root]
        
        -- Compile each project with the development version of the elm compiler.
        mapM_ (compile Dev) projects

        reportResults

-- SETUP

makeRepo :: Repo -> IO () 
makeRepo repo = do
    let name = projectName repo
    projectExists <- doesDirectoryExist name 
    unless projectExists $ 
        void . git $ ["clone", path repo, name]

-- Fetch and prepare a repository containing some component of the Elm Platform.
makeSrcRepo :: FilePath -> Repo -> IO ()
makeSrcRepo root repo = do
    let name = projectName repo
    makeRepo repo
    inDirectory name $ do
        git ["checkout", branch repo, "--quiet"]
    copyFile (root </> "cabal.config.example") (name</> "cabal.config")
       
addSources :: FilePath -> [Repo] -> IO ()
addSources root repos = do
    cabal ["sandbox", "init"]
    mapM_
        (\repo -> cabal ["sandbox", "add-source", root </> projectName repo])
        repos
    void . cabal $ ["install"]


-- COMPILATION

-- Compile an Elm project using the Elm compiler.
compile :: Version -> FilePath -> IO ()
compile version project = do
    root <- getCurrentDirectory
    setCurrentDirectory ("benchmark-projects" </> project) 
    
    -- Remove the build artifacts so that the full project is built each
    -- time we run benchmarking. This ensures that the results of subsequent
    -- benchmarking runs are comparable.
    elmStuffExists <- doesDirectoryExist "elm-stuff/build-artifacts"
    when elmStuffExists (removeDirectoryRecursive "elm-stuff/build-artifacts")

    -- The -p flag turns profiling on.
    let elmMake = rawSystem $ root </> "benchmark-src/elm-make/.cabal-sandbox/bin/elm-make"
    elmMake ["+RTS", "-p"] 

    -- Profiling should produce a file called elm-make.prof.
    copyFile "elm-make.prof" (root 
        </> "benchmark-results" 
        </> (project ++ "." ++ show version ++ ".prof"))

    -- reset
    setCurrentDirectory root
    

-- REPORTING

-- CostCentreDiff is used to group the cost centre results from runs of
-- different compilers.
data CostCentreDiff = CostCentreDiff
    { gold :: Maybe CostCentre
    -- ^ Profiling results for the gold version of the compiler
    , dev :: Maybe CostCentre
    -- ^ Profiling results for the dev version of the compiler
    }

reportResults :: IO ()
reportResults = do
    let goldFilename = "benchmark-results" </> ("elm-todomvc" ++ "." ++ show Gold ++ ".prof")
    goldResults <- Text.readFile goldFilename

    let devFilename = "benchmark-results" </> ("elm-todomvc" ++ "." ++ show Dev ++ ".prof") 
    devResults <- Text.readFile devFilename

    case parseOnly timeAllocProfile goldResults of
        Right goldProfile -> do 
            let goldResults = extractCostCentres goldProfile 

            case parseOnly timeAllocProfile devResults of
                Right devProfile -> do
                    let devResults = extractCostCentres devProfile
                    
                    -- Intersection should have no effect here, since the gold
                    -- and dev maps are both constructed using costCentreNames.
                    reportDiffs $ Map.intersectionWith CostCentreDiff
                        goldResults
                        devResults
                Left error -> reportParseError devFilename error 

        Left error -> reportParseError goldFilename error 

    where
        reportParseError :: FilePath -> String -> IO ()
        reportParseError f errorMsg = putStrLn 
            $ "Error parsing profiling results from "
            ++ f
            ++ ": "
            ++ errorMsg

-- Report the results of profiling in a nice table.
reportDiffs :: Map Text CostCentreDiff -> IO ()
reportDiffs results =
        putStrLn . Table.render unpack id renderMaybe $ resultsTable
    where
        resultsTable :: Table Text String (Maybe Double)
        resultsTable = Table
            (Group SingleLine [ Group NoLine $ map Header (Map.keys results) ])
            (Group DoubleLine [ Group NoLine 
                [ Header "%time in gold"
                , Header "%time in dev"
                ]])
            (map reportDiff $ Map.elems results)

        renderMaybe :: Show a => Maybe a -> String
        renderMaybe = maybe "-" show

        reportDiff :: CostCentreDiff -> [Maybe Double]
        reportDiff costCentre = 
            [ costCentreIndTime <$> gold costCentre
            , costCentreIndTime <$> dev costCentre
            ]

-- Extract cost centres from the given profiling report.
extractCostCentres :: TimeAllocProfile -> Map Text (Maybe CostCentre)
extractCostCentres tree = foldr insertCostCentre Map.empty costCentreNames
    where 
        insertCostCentre :: Text 
            -> Map Text (Maybe CostCentre)
            -> Map Text (Maybe CostCentre)
        insertCostCentre name = Map.insert name (findCostCentre name)

        findCostCentre :: Text -> Maybe CostCentre
        findCostCentre name = find ((== name) . costCentreName)
            $ costCentreNodes (profileCostCentreTree tree)


-- HELPER FUNCTIONS

git :: [String] -> IO ExitCode
git = rawSystem "git"

cabal :: [String] -> IO ExitCode
cabal = rawSystem "cabal" 

successful :: ExitCode -> Bool
successful = (== ExitSuccess)

listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = filter (\d -> d /= "." && d /= "..")
    <$> getDirectoryContents dir

-- inDirectory changes into the specified directory, performs some IO action,
-- and then steps back out into the original directory once the action has been
-- performed.
inDirectory :: FilePath -> IO a -> IO () 
inDirectory dir action = do
    root <- getCurrentDirectory
    setCurrentDirectory dir >> action >> setCurrentDirectory root

