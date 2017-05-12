{-# OPTIONS_GHC -Wall #-}
module Type.State
  ( Solver
  , run
  , SolverState(..)
  , Pool(..)
  , Env
  , modifyEnv
  , addError
  , switchToPool
  , getPool
  , getEnv
  , saveLocalEnv
  , uniqueMark
  , nextRankPool
  , register
  , introduce
  , flatten
  , makeInstance
  , makeCopy
  )
  where

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Text (Text)
import qualified Data.UnionFind.IO as UF

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type


-- SOLVER


type Solver = State.StateT SolverState IO


run :: Solver () -> IO SolverState
run solver =
  State.execStateT solver $
    SS
      { sEnv = Map.empty
      , sSavedEnv = Map.empty
      , sPool = Pool outermostRank []
      , sMark = noMark + 1  -- The mark must never be equal to noMark!
      , sError = []
      }



-- SOLVER STATE


data SolverState =
  SS
    { sEnv :: Env
    , sSavedEnv :: Env
    , sPool :: Pool
    , sMark :: Int
    , sError :: [A.Located Error.Error]
    }


{-| A pool holds a bunch of variables
The rank of each variable is less than or equal to the pool's "maxRank"
The young pool exists to make it possible to identify these vars in constant time.
-}
data Pool =
  Pool
    { maxRank :: Int
    , inhabitants :: [Variable]
    }


type Env = Map.Map Text (A.Located Variable)



-- HELPERS


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


flattenHelp :: Map.Map Text Variable -> Type -> Solver Variable
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
makeCopyHelp (Descriptor content rank mark copy) alreadyCopiedMark variable =
  if mark == alreadyCopiedMark then
      case copy of
        Just copiedVariable ->
            return copiedVariable

        Nothing ->
            error
              "Error copying type variable. This should be impossible.\
              \ Please report this at <https://github.com/elm-lang/elm-compiler/issues>\
              \ with a <http://sscce.org> and information on your OS, how you installed,\
              \ and any other configuration information that might be helpful."

  else if rank /= noRank || not (needsCopy content) then
      return variable

  else
      do  pool <- getPool
          newVar <-
              State.liftIO $ UF.fresh $ Descriptor
                { _content = content -- place holder!
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


          let setContent newContent =
                State.liftIO $ UF.modifyDescriptor newVar $ \desc ->
                  desc { _content = newContent }

          -- Now we recursively copy the content of the variable.
          -- We have already marked the variable as copied, so we
          -- will not repeat this work or crawl this variable again.
          case content of
            Structure term ->
                do  newTerm <- traverseTerm (makeCopy alreadyCopiedMark) term
                    setContent (Structure newTerm)

            Atom _ ->
                return ()

            Var Rigid maybeSuper maybeName ->
                setContent (Var Flex maybeSuper maybeName)

            Var Flex _ _ ->
                return ()

            Alias name args realType ->
                setContent =<< (
                  Alias name
                    <$> mapM (traverse (makeCopy alreadyCopiedMark)) args
                    <*> makeCopy alreadyCopiedMark realType
                )

            Error ->
                return ()

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
