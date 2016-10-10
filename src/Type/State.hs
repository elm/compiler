{-# OPTIONS_GHC -Wall #-}
module Type.State where

import qualified Control.Monad.State as State
import Data.Map ((!))
import qualified Data.Map as Map
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


type Solver = State.StateT SolverState IO


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


modifyEnv :: (Env -> Env) -> Solver ()
modifyEnv f =
    State.modify $ \state -> state { sEnv = f (sEnv state) }


modifyPool :: (Pool -> Pool) -> Solver ()
modifyPool f =
    State.modify $ \state -> state { sPool = f (sPool state) }


addError :: R.Region -> Error.Error -> Solver ()
addError region err =
    State.modify $ \state -> state { sError = A.A region err : sError state }


switchToPool :: Pool -> Solver ()
switchToPool pool =
    modifyPool (\_ -> pool)


getPool :: Solver Pool
getPool =
    State.gets sPool


getEnv :: Solver Env
getEnv =
    State.gets sEnv


saveLocalEnv :: Solver ()
saveLocalEnv =
  do  currentEnv <- getEnv
      State.modify $ \state -> state { sSavedEnv = currentEnv }


uniqueMark :: Solver Int
uniqueMark =
  do  state <- State.get
      let mark = sMark state
      State.put $ state { sMark = mark + 1 }
      return mark


nextRankPool :: Solver Pool
nextRankPool =
  do  pool <- getPool
      return $ Pool { maxRank = maxRank pool + 1, inhabitants = [] }


register :: Variable -> Solver Variable
register variable =
  do  modifyPool $ \pool -> pool { inhabitants = variable : inhabitants pool }
      return variable


introduce :: Variable -> Solver Variable
introduce variable =
  do  pool <- getPool
      State.liftIO $ UF.modifyDescriptor variable (\desc -> desc { _rank = maxRank pool })
      register variable


flatten :: Type -> Solver Variable
flatten term =
  flattenHelp Map.empty term


flattenHelp :: Map.Map String Variable -> Type -> Solver Variable
flattenHelp aliasDict termN =
  case termN of
    PlaceHolder name ->
        return (aliasDict ! name)

    AliasN name args realType ->
        do  flatArgs <- mapM (traverse (flattenHelp aliasDict)) args
            flatVar <- flattenHelp (Map.fromList flatArgs) realType
            pool <- getPool
            variable <-
                State.liftIO . UF.fresh $ Descriptor
                  { _content = Alias name flatArgs flatVar
                  , _rank = maxRank pool
                  , _mark = noMark
                  , _copy = Nothing
                  }
            register variable

    VarN v ->
        return v

    TermN term1 ->
        do  variableTerm <- traverseTerm (flattenHelp aliasDict) term1
            pool <- getPool
            variable <-
                State.liftIO . UF.fresh $ Descriptor
                  { _content = Structure variableTerm
                  , _rank = maxRank pool
                  , _mark = noMark
                  , _copy = Nothing
                  }
            register variable


makeInstance :: Variable -> Solver Variable
makeInstance var =
  do  alreadyCopiedMark <- uniqueMark
      freshVar <- makeCopy alreadyCopiedMark var
      _ <- restore alreadyCopiedMark var
      return freshVar


makeCopy :: Int -> Variable -> Solver Variable
makeCopy alreadyCopiedMark variable =
  do  desc <- State.liftIO $ UF.descriptor variable
      makeCopyHelp desc alreadyCopiedMark variable


makeCopyHelp :: Descriptor -> Int -> Variable -> Solver Variable
makeCopyHelp descriptor alreadyCopiedMark variable =
  if _mark descriptor == alreadyCopiedMark then
      case _copy descriptor of
        Just copiedVariable ->
            return copiedVariable

        Nothing ->
            error
              "Error copying type variable. This should be impossible.\
              \ Please report this at <https://github.com/elm-lang/elm-compiler/issues>\
              \ with a <http://sscce.org> and information on your OS, how you installed,\
              \ and any other configuration information that might be helpful."

  else if _rank descriptor /= noRank || not (needsCopy (_content descriptor)) then
      return variable

  else
      do  pool <- getPool
          newVar <-
              State.liftIO $ UF.fresh $ Descriptor
                { _content = error "will be filled in soon!"
                , _rank = maxRank pool
                , _mark = noMark
                , _copy = Nothing
                }
          _ <- register newVar

          -- Link the original variable to the new variable. This lets us
          -- avoid making multiple copies of the variable we are instantiating.
          --
          -- Need to do this before recursively copying the content of
          -- the variable to avoid looping on cyclic terms.
          State.liftIO $ UF.modifyDescriptor variable $ \desc ->
              desc { _mark = alreadyCopiedMark, _copy = Just newVar }

          -- Now we recursively copy the content of the variable.
          -- We have already marked the variable as copied, so we
          -- will not repeat this work or crawl this variable again.
          let oldContent = _content descriptor
          newContent <-
              case oldContent of
                Structure term ->
                    Structure <$> traverseTerm (makeCopy alreadyCopiedMark) term

                Atom _ ->
                    return oldContent

                Var Rigid maybeSuper maybeName ->
                    return (Var Flex maybeSuper maybeName)

                Var Flex _ _ ->
                    return oldContent

                Alias name args realType ->
                    Alias name
                        <$> mapM (traverse (makeCopy alreadyCopiedMark)) args
                        <*> makeCopy alreadyCopiedMark realType

                Error ->
                    return oldContent

          State.liftIO $ UF.modifyDescriptor newVar $ \desc ->
              desc { _content = newContent }

          return newVar


needsCopy :: Content -> Bool
needsCopy content =
  case content of
    Structure _ ->
        True

    Atom _ ->
        False

    Var _ _ _ ->
        True

    Alias _ _ _ ->
        True

    Error ->
        False



restore :: Int -> Variable -> Solver Variable
restore alreadyCopiedMark variable =
  do  desc <- State.liftIO $ UF.descriptor variable
      if _mark desc /= alreadyCopiedMark
        then
          return variable

        else
          do  restoredContent <-
                  restoreContent alreadyCopiedMark (_content desc)
              State.liftIO $ UF.setDescriptor variable $ Descriptor
                  { _content = restoredContent
                  , _rank = noRank
                  , _mark = noMark
                  , _copy = Nothing
                  }
              return variable



restoreContent :: Int -> Content -> Solver Content
restoreContent alreadyCopiedMark content =
  let
    go = restore alreadyCopiedMark
  in
  case content of
    Structure term ->
        Structure <$> traverseTerm go term

    Atom _ ->
        return content

    Var _ _ _ ->
        return content

    Alias name args var ->
        Alias name
          <$> mapM (traverse go) args
          <*> go var

    Error ->
        return content



-- TERM TRAVERSAL


traverseTerm :: (Monad f, Applicative f) => (a -> f b) -> Term1 a -> f (Term1 b)
traverseTerm f term =
  case term of
    App1 a b ->
        App1 <$> f a <*> f b

    Fun1 a b ->
        Fun1 <$> f a <*> f b

    EmptyRecord1 ->
        return EmptyRecord1

    Record1 fields ext ->
        Record1 <$> traverse f fields <*> f ext
