{-# LANGUAGE FlexibleContexts #-}
module Type.State where

import Control.Applicative ( Applicative, (<$>), (<*>), (<|>) )
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.UnionFind.IO as UF

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type


-- Pool
-- Holds a bunch of variables
-- The rank of each variable is less than or equal to the pool's "maxRank"
-- The young pool exists to make it possible to identify these vars in constant time.

data Pool = Pool
    { maxRank :: Int
    , inhabitants :: [Variable]
    }


emptyPool :: Pool
emptyPool =
    Pool
    { maxRank = outermostRank
    , inhabitants = []
    }


type Env = Map.Map String (A.Located Variable)


-- Keeps track of the environment, type variable pool, and a list of errors
data SolverState = SS
    { sEnv :: Env
    , sSavedEnv :: Env
    , sPool :: Pool
    , sMark :: Int
    , sError :: [A.Located Error.Error]
    }


initialState :: SolverState
initialState =
    SS
    { sEnv = Map.empty
    , sSavedEnv = Map.empty
    , sPool = emptyPool
    , sMark = noMark + 1  -- The mark must never be equal to noMark!
    , sError = []
    }


modifyEnv :: (MonadState SolverState m) => (Env -> Env) -> m ()
modifyEnv f =
    modify $ \state -> state { sEnv = f (sEnv state) }


modifyPool :: (MonadState SolverState m) => (Pool -> Pool) -> m ()
modifyPool f =
    modify $ \state -> state { sPool = f (sPool state) }


addError :: R.Region -> Error.Error -> StateT SolverState IO ()
addError region err =
    modify $ \state -> state { sError = A.A region err : sError state }


switchToPool :: (MonadState SolverState m) => Pool -> m ()
switchToPool pool =
    modifyPool (\_ -> pool)


getPool :: StateT SolverState IO Pool
getPool =
    gets sPool


getEnv :: StateT SolverState IO Env
getEnv =
    gets sEnv


saveLocalEnv :: StateT SolverState IO ()
saveLocalEnv =
  do  currentEnv <- getEnv
      modify $ \state -> state { sSavedEnv = currentEnv }


uniqueMark :: StateT SolverState IO Int
uniqueMark =
  do  state <- get
      let mark = sMark state
      put $ state { sMark = mark + 1 }
      return mark


nextRankPool :: StateT SolverState IO Pool
nextRankPool =
  do  pool <- getPool
      return $ Pool { maxRank = maxRank pool + 1, inhabitants = [] }


register :: Variable -> StateT SolverState IO Variable
register variable =
  do  modifyPool $ \pool -> pool { inhabitants = variable : inhabitants pool }
      return variable


introduce :: Variable -> StateT SolverState IO Variable
introduce variable =
  do  pool <- getPool
      liftIO $ UF.modifyDescriptor variable (\desc -> desc { rank = maxRank pool })
      register variable


flatten :: Type -> StateT SolverState IO Variable
flatten term =
  flattenHelp Map.empty term


flattenHelp :: Map.Map String Variable -> Type -> StateT SolverState IO Variable
flattenHelp aliasDict term =
  case term of
    PlaceHolder name ->
      return (aliasDict ! name)

    VarN maybeAlias v ->
      do  maybeAlias' <- Traversable.traverse flattenAlias maybeAlias
          liftIO $ UF.modifyDescriptor v $ \desc ->
              desc { alias = maybeAlias' <|> alias desc }
          return v

    TermN maybeAlias subTerm ->
      do  maybeAlias' <- Traversable.traverse flattenAlias maybeAlias
          let localDict = maybe aliasDict (Map.fromList . snd) maybeAlias'
          flatStructure <- traverseTerm (flattenHelp localDict) subTerm
          pool <- getPool
          var <-
              liftIO . UF.fresh $ Descriptor
                { structure = Just flatStructure
                , rank = maxRank pool
                , flex = Flexible
                , name = Nothing
                , copy = Nothing
                , mark = noMark
                , alias = maybeAlias'
                }
          register var
  where
    flattenAlias (name, args) =
        let flattenPair (arg, subTerm) =
              (,) arg <$> flattenHelp aliasDict subTerm
        in
            (,) name <$> mapM flattenPair args




makeInstance :: Variable -> StateT SolverState IO Variable
makeInstance var =
  do  alreadyCopied <- uniqueMark
      freshVar <- makeCopy alreadyCopied var
      restore alreadyCopied var
      return freshVar


makeCopy :: Int -> Variable -> StateT SolverState IO Variable
makeCopy alreadyCopied variable = do
  desc <- liftIO $ UF.descriptor variable
  case () of
   () | mark desc == alreadyCopied ->
          case copy desc of
            Just v ->
                return v
            Nothing ->
                error $
                  "Error copying type variable. This should be impossible." ++
                  " Please report an error to the github repo!"

      | rank desc /= noRank || flex desc == Constant ->
          return variable

      | otherwise ->
          do  pool <- getPool
              newVar <-
                  liftIO $ UF.fresh $ Descriptor
                    { structure = Nothing
                    , rank = maxRank pool
                    , mark = noMark
                    , flex =
                        case flex desc of
                          Is s -> Is s
                          _ -> Flexible
                    , copy = Nothing
                    , name =
                        case flex desc of
                          Rigid -> Nothing
                          _ -> name desc
                    , alias = Nothing
                    }
              register newVar

              -- Link the original variable to the new variable. This lets us
              -- avoid making multiple copies of the variable we are instantiating.
              --
              -- Need to do this before recursively copying the structure of
              -- the variable to avoid looping on cyclic terms.
              liftIO $ UF.modifyDescriptor variable $ \desc ->
                  desc { mark = alreadyCopied, copy = Just newVar }

              -- Now we recursively copy the structure of the variable.
              -- We have already marked the variable as copied, so we
              -- will not repeat this work or crawl this variable again.
              case structure desc of
                Nothing ->
                  return newVar

                Just term ->
                  do  newTerm <- traverseTerm (makeCopy alreadyCopied) term
                      liftIO $ UF.modifyDescriptor newVar $ \desc ->
                          desc { structure = Just newTerm }
                      return newVar


restore :: Int -> Variable -> StateT SolverState IO Variable
restore alreadyCopied variable =
  do  desc <- liftIO $ UF.descriptor variable
      case mark desc /= alreadyCopied of
        True -> return variable
        False ->
          do  restoredStructure <-
                  Traversable.traverse (traverseTerm (restore alreadyCopied)) (structure desc)
              liftIO $ UF.modifyDescriptor variable $ \desc ->
                  desc { mark = noMark, rank = noRank, structure = restoredStructure }
              return variable


traverseTerm :: (Monad f, Applicative f) => (a -> f b) -> Term1 a -> f (Term1 b)
traverseTerm f term =
  case term of
    App1 a b ->
        App1 <$> f a <*> f b

    Fun1 a b ->
        Fun1 <$> f a <*> f b

    Var1 x ->
        Var1 <$> f x

    EmptyRecord1 ->
        return EmptyRecord1

    Record1 fields ext ->
        Record1
          <$> Traversable.traverse (mapM f) fields
          <*> f ext
