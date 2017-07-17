{-# OPTIONS_GHC -Wall #-}
module Type.State
  ( Solver
  , run
  , State(..)
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
  , copy
  )
  where


import Control.Monad (liftM, liftM2)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Text (Text)

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type as Type
import qualified Type.UnionFind as UF



-- SOLVER


newtype Solver a =
  Solver { _solve :: State -> IO (a, State) }



-- RUN SOLVER


run :: Solver () -> IO State
run (Solver solver) =
  do  (_, state) <- solver initialState
      return state


initialState :: State
initialState =
  SS
    { sEnv = Map.empty
    , sSavedEnv = Map.empty
    , sPool = Pool outermostRank []
    , sMark = Type.nextMark noMark  -- The mark must never be equal to noMark!
    , sError = []
    }



-- SOLVER STATE


data State =
  SS
    { sEnv :: Env
    , sSavedEnv :: Env
    , sPool :: Pool
    , sMark :: Mark
    , sError :: [A.Located Error.Error]
    }


{-| A pool holds a bunch of variables
The rank of each variable is less than or equal to the pool's "maxRank"
The young pool exists to make it possible to identify these vars in constant time.
-}
data Pool =
  Pool
    { _maxRank :: Int
    , _inhabitants :: [Variable]
    }


type Env = Map.Map Text (A.Located Variable)



-- HELPERS


{-# INLINE modifyEnv #-}
modifyEnv :: (Env -> Env) -> Solver ()
modifyEnv f =
  Solver $ \state ->
    return ( (), state { sEnv = f (sEnv state) } )


{-# INLINE modifyPool #-}
modifyPool :: (Pool -> Pool) -> Solver ()
modifyPool f =
  Solver $ \state ->
    return ( (), state { sPool = f (sPool state) } )


{-# INLINE addError #-}
addError :: R.Region -> Error.Error -> Solver ()
addError region err =
  Solver $ \state ->
    return ( (), state { sError = A.A region err : sError state } )


{-# INLINE switchToPool #-}
switchToPool :: Pool -> Solver ()
switchToPool pool =
  modifyPool (\_ -> pool)


{-# INLINE getPool #-}
getPool :: Solver Pool
getPool =
  Solver $ \state ->
    return ( sPool state, state )


{-# INLINE getMaxRank #-}
getMaxRank :: Solver Int
getMaxRank =
  Solver $ \state@(SS _ _ (Pool maxRank _) _ _) ->
    return ( maxRank, state )


{-# INLINE getEnv #-}
getEnv :: Solver Env
getEnv =
  Solver $ \state ->
    return ( sEnv state, state )


{-# INLINE saveLocalEnv #-}
saveLocalEnv :: Solver ()
saveLocalEnv =
  Solver $ \state ->
    return ( (), state { sSavedEnv = sEnv state } )


{-# INLINE uniqueMark #-}
uniqueMark :: Solver Mark
uniqueMark =
  Solver $ \(SS env savedEnv pool mark errors) ->
    return ( mark, SS env savedEnv pool (Type.nextMark mark) errors )


{-# INLINE nextRankPool #-}
nextRankPool :: Solver Pool
nextRankPool =
  Solver $ \state@(SS _ _ (Pool maxRank _) _ _) ->
    return ( Pool (maxRank + 1) [], state )



-- REGISTER VARIABLES


{-# INLINE register #-}
register :: Variable -> Solver Variable
register variable =
  {-# SCC elm_compiler_type_register #-}
  Solver $ \state ->
    let
      (Pool maxRank inhabitants) =
        sPool state
    in
      return ( variable, state { sPool = Pool maxRank (variable : inhabitants) } )


{-# INLINE introduce #-}
introduce :: [Variable] -> Solver ()
introduce variables =
  {-# SCC elm_compiler_type_introduce #-}
  Solver $ \(SS env savedEnv (Pool maxRank inhabitants) mark errors) ->
    do  let toMaxRank (Descriptor c _ m cp) = Descriptor c maxRank m cp
        mapM_ (\var -> UF.modifyDescriptor var toMaxRank) variables
        let newPool = Pool maxRank (variables ++ inhabitants)
        return ( (), SS env savedEnv newPool mark errors )



-- FLATTEN


flatten :: Type -> Solver Variable
flatten term =
  {-# SCC elm_compiler_type_flatten #-}
  do  maxRank <- getMaxRank
      flattenHelp maxRank Map.empty term


flattenHelp :: Int -> Map.Map Text Variable -> Type -> Solver Variable
flattenHelp maxRank aliasDict termN =
  case termN of
    PlaceHolder name ->
        return (aliasDict ! name)

    AliasN name args realType ->
        do  flatArgs <- mapM (traverse (flattenHelp maxRank aliasDict)) args
            flatVar <- flattenHelp maxRank (Map.fromList flatArgs) realType
            makeFlatType maxRank (Alias name flatArgs flatVar)

    VarN v ->
        return v

    AppN name args ->
        do  flatArgs <- traverse (flattenHelp maxRank aliasDict) args
            makeFlatType maxRank (Structure (App1 name flatArgs))

    FunN a b ->
        do  flatA <- flattenHelp maxRank aliasDict a
            flatB <- flattenHelp maxRank aliasDict b
            makeFlatType maxRank (Structure (Fun1 flatA flatB))

    EmptyRecordN ->
        makeFlatType maxRank (Structure EmptyRecord1)

    RecordN fields ext ->
        do  flatFields <- traverse (flattenHelp maxRank aliasDict) fields
            flatExt <- flattenHelp maxRank aliasDict ext
            makeFlatType maxRank (Structure (Record1 flatFields flatExt))


makeFlatType :: Int -> Content -> Solver Variable
makeFlatType maxRank content =
  register =<< liftIO (UF.fresh (Descriptor content maxRank noMark Nothing))



-- COPY


copy :: Variable -> Solver Variable
copy var =
  {-# SCC elm_compiler_type_copy #-}
  do  maxRank <- getMaxRank
      copyVar <- copyHelp maxRank var
      _ <- restore var
      return copyVar


copyHelp :: Int -> Variable -> Solver Variable
copyHelp maxRank variable =
  do  (Descriptor content rank _mark maybeCopy) <- liftIO $ UF.descriptor variable
      case maybeCopy of
        Just copiedVariable ->
          return copiedVariable

        Nothing ->
          if rank /= noRank then
            return variable

          else
            do  let mkCopyDesc cont = Descriptor cont maxRank noMark Nothing
                newVar <- liftIO $ UF.fresh $ mkCopyDesc content
                _ <- register newVar

                -- Link the original variable to the new variable. This lets us
                -- avoid making multiple copies of the variable we are instantiating.
                --
                -- Need to do this before recursively copying the content of
                -- the variable to avoid looping on cyclic terms.
                liftIO $ UF.setDescriptor variable $
                  Descriptor content rank noMark (Just newVar)

                -- Now we recursively copy the content of the variable.
                -- We have already marked the variable as copied, so we
                -- will not repeat this work or crawl this variable again.
                case content of
                  Structure term ->
                    do  newTerm <- traverseFlatType (copyHelp maxRank) term
                        liftIO $ UF.setDescriptor newVar $ mkCopyDesc (Structure newTerm)

                  Var Rigid maybeSuper maybeName ->
                    liftIO $ UF.setDescriptor newVar $ mkCopyDesc $ Var Flex maybeSuper maybeName

                  Var Flex _ _ ->
                    return ()

                  Alias name args realType ->
                    do  newArgs <- mapM (traverse (copyHelp maxRank)) args
                        newRealType <- copyHelp maxRank realType
                        liftIO $ UF.setDescriptor newVar $ mkCopyDesc (Alias name newArgs newRealType)

                  Error _ ->
                    return ()

                return newVar



-- RESTORE


restore :: Variable -> Solver Variable
restore variable =
  do  (Descriptor content _rank _mark maybeCopy) <- liftIO $ UF.descriptor variable
      case maybeCopy of
        Nothing ->
          return variable

        Just _ ->
          do  restoredContent <- restoreContent content
              liftIO $ UF.setDescriptor variable $
                Descriptor restoredContent noRank noMark Nothing
              return variable


restoreContent :: Content -> Solver Content
restoreContent content =
  case content of
    Structure term ->
        Structure <$> traverseFlatType restore term

    Var _ _ _ ->
        return content

    Alias name args var ->
        Alias name
          <$> mapM (traverse restore) args
          <*> restore var

    Error _ ->
        return content



-- TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> Solver Variable) -> FlatType -> Solver FlatType
traverseFlatType f flatType =
  case flatType of
    App1 name args ->
        liftM (App1 name) (traverse f args)

    Fun1 a b ->
        liftM2 Fun1 (f a) (f b)

    EmptyRecord1 ->
        pure EmptyRecord1

    Record1 fields ext ->
        liftM2 Record1 (traverse f fields) (f ext)



-- INSTANCES


instance Functor Solver where
  {-# INLINE fmap #-}
  fmap f (Solver solver) =
    Solver $ \s1 ->
      fmap (\(a, s2) -> (f a, s2)) (solver s1)


instance Applicative Solver where
  {-# INLINE pure #-}
  pure a =
    Solver $ \s -> return (a, s)

  {-# INLINE (<*>) #-}
  Solver solverFunc <*> Solver solverValue =
    Solver $ \s1 ->
      do  (f, s2) <- solverFunc s1
          (x, s3) <- solverValue s2
          return (f x, s3)


instance Monad Solver where
  {-# INLINE (>>=) #-}
  Solver solver >>= k =
    Solver $ \s1 ->
      do  (a, s2) <- solver s1
          (b, s3) <- _solve (k a) s2
          return (b, s3)

  {-# INLINE fail #-}
  fail msg =
    Solver $ \_ -> fail msg


instance MonadIO Solver where
  {-# INLINE liftIO #-}
  liftIO io =
    Solver $ \state ->
      do  a <- io
          return (a, state)
