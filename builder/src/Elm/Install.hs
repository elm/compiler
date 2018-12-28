{-# LANGUAGE OverloadedStrings #-}
module Elm.Install
  ( install
  )
  where


import Control.Monad (filterM, foldM, forM, msum, void)
import Control.Monad.Except (catchError, lift, liftIO)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map

import qualified Elm.Package as Pkg

import qualified Deps.Cache as Cache
import Deps.Explorer (Explorer)
import qualified Deps.Explorer as Explorer
import qualified Deps.Verify as Verify
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as C
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Root as Root
import qualified Elm.Version as V
import Reporting.Doc ((<>), (<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Install as E
import qualified Reporting.Task as Task



-- INSTALL


install :: Pkg.Name -> Task.Task ()
install pkg =
  do  (root, oldProject) <- Root.unsafeGet
      registry <- Cache.optionalUpdate

      case oldProject of
        Project.App info ->
          do  changes <- makeAppPlan registry pkg info
              attemptElmJsonChange root oldProject V.toChars changes

        Project.Pkg info ->
          do  changes <- makePkgPlan registry pkg info
              attemptElmJsonChange root oldProject C.toChars changes


attemptElmJsonChange :: FilePath -> Project.Project -> (a -> String) -> Changes a -> Task.Task ()
attemptElmJsonChange root oldProject toChars changes =
  let
    attempt newProject question =
      do  approved <- Task.getApproval question
          if approved
            then upgrade newProject `catchError` revert
            else liftIO $ putStrLn "Okay, I did not change anything!"

    upgrade newProject =
      do  liftIO $ Project.write root newProject
          void $ Verify.verify root newProject

    revert err =
      do  liftIO $ Project.write root oldProject
          Task.throw err
  in
  case changes of
    AlreadyInstalled ->
      do  liftIO $ putStrLn "It is already installed!"

    PromoteTrans newProject ->
      attempt newProject $
        D.vcat
         [ D.fillSep
            ["I","found","it","in","your","elm.json","file,"
            ,"but","in","the",D.dullyellow "\"indirect\"","dependencies."
            ]
         , D.fillSep
            ["Should","I","move","it","into",D.green "\"direct\""
            ,"dependencies","for","more","general","use?","[Y/n]: "
            ]
         ]

    PromoteTest newProject ->
      attempt newProject $
        D.vcat
         [ D.fillSep
            ["I","found","it","in","your","elm.json","file,"
            ,"but","in","the",D.dullyellow "\"test-dependencies\"","field."
            ]
         , D.fillSep
            ["Should","I","move","it","into",D.green "\"dependencies\""
            ,"for","more","general","use?","[Y/n]: "
            ]
         ]

    Changes changeDict newProject ->
      let
        widths = Map.foldrWithKey (widen toChars) (Widths 0 0 0) changeDict
        changeDocs = Map.foldrWithKey (addChange toChars widths) (Docs [] [] []) changeDict
      in
      attempt newProject $ D.vcat $
        [ "Here is my plan:"
        , viewChangeDocs changeDocs
        , ""
        , "Would you like me to update your elm.json accordingly? [Y/n]: "
        ]



-- MAKE PLANS


data Changes vsn
  = AlreadyInstalled
  | PromoteTest Project.Project
  | PromoteTrans Project.Project
  | Changes (Map.Map Pkg.Name (Change vsn)) Project.Project


makeAppPlan :: Cache.PackageRegistry -> Pkg.Name -> Project.AppInfo -> Task.Task (Changes V.Version)
makeAppPlan registry pkg info@(Project.AppInfo _ _ depsDirect depsTrans testDirect testTrans) =
  if Map.member pkg depsDirect then
    return AlreadyInstalled
  else
    do  (AppAnswer old new newInfo) <- toAppAnswer registry pkg info

        return $
          if Map.member pkg depsTrans then
            PromoteTrans (Project.App newInfo)

          else if Map.member pkg testDirect || Map.member pkg testTrans then
            PromoteTest (Project.App newInfo)

          else
            Changes (detectChanges old new) (Project.App newInfo)


makePkgPlan :: Cache.PackageRegistry -> Pkg.Name -> Project.PkgInfo -> Task.Task (Changes C.Constraint)
makePkgPlan registry pkg info@(Project.PkgInfo _ _ _ _ _ deps test _) =
  if Map.member pkg deps then
    return AlreadyInstalled
  else
    case Map.lookup pkg test of
      Just con ->
        return $ PromoteTest $ Project.Pkg $
          info
            { Project._pkg_deps = Map.insert pkg con deps
            , Project._pkg_test_deps = Map.delete pkg test
            }

      Nothing ->
        do  changes <- addToPkg registry pkg info
            let news = Map.mapMaybe keepNew changes
            return $ Changes changes $ Project.Pkg $
              info
                { Project._pkg_deps = addNews (Just pkg) news deps
                , Project._pkg_test_deps = addNews Nothing news test
                }


addNews :: Maybe Pkg.Name -> Map.Map Pkg.Name a -> Map.Map Pkg.Name a -> Map.Map Pkg.Name a
addNews pkg new old =
  Map.merge
    Map.preserveMissing
    (Map.mapMaybeMissing (\k v -> if Just k == pkg then Just v else Nothing))
    (Map.zipWithMatched (\_ _ n -> n))
    old
    new



-- CHANGES


data Change a
  = Insert a
  | Change a a
  | Remove a


detectChanges :: (Eq a) => Map.Map Pkg.Name a -> Map.Map Pkg.Name a -> Map.Map Pkg.Name (Change a)
detectChanges old new =
  Map.merge
    (Map.mapMissing (\_ v -> Remove v))
    (Map.mapMissing (\_ v -> Insert v))
    (Map.zipWithMaybeMatched keepChange)
    old
    new


keepChange :: (Eq v) => k -> v -> v -> Maybe (Change v)
keepChange _ old new =
  if old == new then
    Nothing
  else
    Just (Change old new)


keepNew :: Change a -> Maybe a
keepNew change =
  case change of
    Insert a ->
      Just a

    Change _ a ->
      Just a

    Remove _ ->
      Nothing



-- ADD TO APP


data AppAnswer =
  AppAnswer
    { _old :: Map.Map Pkg.Name V.Version
    , _new :: Map.Map Pkg.Name V.Version
    , _info :: Project.AppInfo
    }


toAppAnswer :: Cache.PackageRegistry -> Pkg.Name -> Project.AppInfo -> Task.Task AppAnswer
toAppAnswer registry pkg info@(Project.AppInfo _ _ depsDirect depsTrans testDirect testTrans) =
  Explorer.run registry $
    do  Explorer.exists pkg
        result <- Solver.run (toAppAnswerHelp pkg info)
        case result of
          Just answer ->
            return answer

          Nothing ->
            do  badNames <-
                  filterM isBadElm $
                    pkg : Map.keys depsDirect
                    ++ Map.keys depsTrans
                    ++ Map.keys testDirect
                    ++ Map.keys testTrans
                lift $ Task.throw (Exit.Install (E.NoSolution badNames))


toAppAnswerHelp :: Pkg.Name -> Project.AppInfo -> Solver.Solver AppAnswer
toAppAnswerHelp pkg (Project.AppInfo elm srcDirs depsDirect depsTrans testDirect testTrans) =
  let
    directs =
      Map.union depsDirect testDirect

    directAndTrans =
      Map.union directs (Map.union depsTrans testTrans)

    try toConstraint deps =
      do  solution <- Solver.solve $ Map.insert pkg C.anything (Map.map toConstraint deps)
          let newDepsDirect = Map.intersection solution (Map.insert pkg V.one depsDirect)
          newDeps <- lift $ foldM (collectTransitive solution) Map.empty (Map.keys newDepsDirect)
          return $
            AppAnswer
              { _old = directAndTrans
              , _new = solution
              , _info =
                  Project.AppInfo
                    elm
                    srcDirs
                    newDepsDirect
                    (Map.difference newDeps newDepsDirect)
                    (Map.intersection solution testDirect)
                    (Map.difference (Map.difference solution newDeps) testDirect)
              }
  in
  msum
    [ try C.exactly directAndTrans
    , try C.exactly directs
    , try C.untilNextMinor directs
    , try C.untilNextMajor directs
    , try (\_ -> C.anything) directs
    ]


collectTransitive
  :: Map.Map Pkg.Name V.Version
  -> Map.Map Pkg.Name V.Version
  -> Pkg.Name
  -> Explorer (Map.Map Pkg.Name V.Version)
collectTransitive solution state name =
  do  let version = solution ! name
      (Explorer.Info _ trans) <- Explorer.getConstraints name version
      foldM
        (collectTransitive solution)
        (Map.insert name version state)
        (Map.keys trans)





-- ADD TO PKG


addToPkg :: Cache.PackageRegistry -> Pkg.Name -> Project.PkgInfo -> Task.Task (Map.Map Pkg.Name (Change C.Constraint))
addToPkg registry pkg info@(Project.PkgInfo _ _ _ _ _ deps tests _) =
  Explorer.run registry $
    do  Explorer.exists pkg
        let old = Map.union deps tests
        result <- Solver.run (addToPkgHelp pkg info)
        case result of
          Just new ->
            return $ detectChanges old new

          Nothing ->
            do  let pkgs = Map.keys deps ++ Map.keys tests
                badNames <- filterM isBadElm (pkg : pkgs)
                lift $ Task.throw (Exit.Install (E.NoSolution badNames))


addToPkgHelp :: Pkg.Name -> Project.PkgInfo -> Solver.Solver (Map.Map Pkg.Name C.Constraint)
addToPkgHelp pkg (Project.PkgInfo _ _ _ _ _ deps tests _) =
  do  let directs = Map.union deps tests
      let newCons = Map.insert pkg C.anything directs
      solution <- Solver.solve newCons
      let con = C.untilNextMajor (solution ! pkg)
      return $ Map.insert pkg con directs



-- FAILURE HINTS


isBadElm :: Pkg.Name -> Explorer Bool
isBadElm name =
  do  versions <- Explorer.getVersions name

      elmVersions <- forM versions $ \vsn ->
        Explorer._elm <$> Explorer.getConstraints name vsn

      return (not (any C.goodElm elmVersions))



-- VIEW CHANGE DOCS


data ChangeDocs =
  Docs
    { _doc_inserts :: [D.Doc]
    , _doc_changes :: [D.Doc]
    , _doc_removes :: [D.Doc]
    }


viewChangeDocs :: ChangeDocs -> D.Doc
viewChangeDocs (Docs inserts changes removes) =
  D.indent 2 $ D.vcat $ concat $
    [ viewNonZero "Add:"    inserts
    , viewNonZero "Change:" changes
    , viewNonZero "Remove:" removes
    ]


viewNonZero :: String -> [D.Doc] -> [D.Doc]
viewNonZero title entries =
  if null entries then
    []
  else
    [ ""
    , D.fromChars title
    , D.indent 2 (D.vcat entries)
    ]



-- VIEW CHANGE


addChange :: (a -> String) -> Widths -> Pkg.Name -> Change a -> ChangeDocs -> ChangeDocs
addChange toChars widths name change (Docs inserts changes removes) =
  case change of
    Insert new ->
      Docs (viewInsert toChars widths name new : inserts) changes removes

    Change old new ->
      Docs inserts (viewChange toChars widths name old new : changes) removes

    Remove old ->
      Docs inserts changes (viewRemove toChars widths name old : removes)


viewInsert :: (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewInsert toChars (Widths nameWidth leftWidth _) name new =
  viewName nameWidth name <+> pad leftWidth (toChars new)


viewChange :: (a -> String) -> Widths -> Pkg.Name -> a -> a -> D.Doc
viewChange toChars (Widths nameWidth leftWidth rightWidth) name old new =
  D.hsep
    [ viewName nameWidth name
    , pad leftWidth (toChars old)
    , "=>"
    , pad rightWidth (toChars new)
    ]


viewRemove :: (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewRemove toChars (Widths nameWidth leftWidth _) name old =
  viewName nameWidth name <+> pad leftWidth (toChars old)


viewName :: Int -> Pkg.Name -> D.Doc
viewName width name =
  D.fill (width + 3) (D.fromUtf8 (Pkg.toString name))


pad :: Int -> String -> D.Doc
pad width string =
  D.fromChars (replicate (width - length string) ' ') <> D.fromChars string



-- WIDTHS


data Widths =
  Widths
    { _name :: !Int
    , _left :: !Int
    , _right :: !Int
    }


widen :: (a -> String) -> Pkg.Name -> Change a -> Widths -> Widths
widen toChars pkg change (Widths name left right) =
  let
    toLength a =
      length (toChars a)

    newName =
      max name (length (Pkg.toChars pkg))
  in
    case change of
      Insert new ->
        Widths newName (max left (toLength new)) right

      Change old new ->
        Widths newName (max left (toLength old)) (max right (toLength new))

      Remove old ->
        Widths newName (max left (toLength old)) right
