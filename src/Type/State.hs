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
  , makeInstance
  , makeCopy
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
import Type.Type
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
    , sMark = noMark + 1  -- The mark must never be equal to noMark!
    , sError = []
    }



-- SOLVER STATE


data State =
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
uniqueMark :: Solver Int
uniqueMark =
  Solver $ \state ->
    let
      mark =
        sMark state
    in
      return ( mark, state { sMark = mark + 1 } )


{-# INLINE nextRankPool #-}
nextRankPool :: Solver Pool
nextRankPool =
  Solver $ \state ->
    let
      (Pool maxRank _) =
        sPool state
    in
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
introduce :: Variable -> Solver Variable
introduce variable =
  {-# SCC elm_compiler_type_introduce #-}
  Solver $ \state ->
    let
      (Pool maxRank inhabitants) =
        sPool state
    in
      do  UF.modifyDescriptor variable (\desc -> desc { _rank = maxRank })
          return ( variable, state { sPool = Pool maxRank (variable : inhabitants) } )



-- FLATTEN


flatten :: Type -> Solver Variable
flatten term =
  {-# SCC elm_compiler_type_flatten #-}
  flattenHelp Map.empty term


flattenHelp :: Map.Map Text Variable -> Type -> Solver Variable
flattenHelp aliasDict termN =
  case termN of
    PlaceHolder name ->
        return (aliasDict ! name)

    AliasN name args realType ->
        do  flatArgs <- mapM (traverse (flattenHelp aliasDict)) args
            flatVar <- flattenHelp (Map.fromList flatArgs) realType
            makeFlatType (Alias name flatArgs flatVar)

    VarN v ->
        return v

    AppN name args ->
        do  flatArgs <- traverse (flattenHelp aliasDict) args
            makeFlatType (Structure (App1 name flatArgs))

    FunN a b ->
        do  flatA <- flattenHelp aliasDict a
            flatB <- flattenHelp aliasDict b
            makeFlatType (Structure (Fun1 flatA flatB))

    EmptyRecordN ->
        makeFlatType (Structure EmptyRecord1)

    RecordN fields ext ->
        do  flatFields <- traverse (flattenHelp aliasDict) fields
            flatExt <- flattenHelp aliasDict ext
            makeFlatType (Structure (Record1 flatFields flatExt))


makeFlatType :: Content -> Solver Variable
makeFlatType content =
  do  pool <- getPool
      variable <- liftIO $ UF.fresh $
        Descriptor
          { _content = content
          , _rank = _maxRank pool
          , _mark = noMark
          , _copy = Nothing
          }
      register variable



-- MAKE INSTANCE


makeInstance :: Variable -> Solver Variable
makeInstance var =
  do  alreadyCopiedMark <- uniqueMark
      freshVar <- makeCopy alreadyCopiedMark var
      _ <- restore alreadyCopiedMark var
      return freshVar



-- MAKE COPY


makeCopy :: Int -> Variable -> Solver Variable
makeCopy alreadyCopiedMark variable =
  {-# SCC elm_compiler_type_copy #-}
  do  desc <- liftIO $ UF.descriptor variable
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
              liftIO $ UF.fresh $ Descriptor
                { _content = content -- place holder!
                , _rank = _maxRank pool
                , _mark = noMark
                , _copy = Nothing
                }
          _ <- register newVar

          -- Link the original variable to the new variable. This lets us
          -- avoid making multiple copies of the variable we are instantiating.
          --
          -- Need to do this before recursively copying the content of
          -- the variable to avoid looping on cyclic terms.
          liftIO $ UF.modifyDescriptor variable $ \desc ->
              desc { _mark = alreadyCopiedMark, _copy = Just newVar }


          let setContent newContent =
                liftIO $ UF.modifyDescriptor newVar $ \desc ->
                  desc { _content = newContent }

          -- Now we recursively copy the content of the variable.
          -- We have already marked the variable as copied, so we
          -- will not repeat this work or crawl this variable again.
          case content of
            Structure term ->
                do  newTerm <- traverseFlatType (makeCopy alreadyCopiedMark) term
                    setContent (Structure newTerm)

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

            Error _ ->
                return ()

          return newVar


needsCopy :: Content -> Bool
needsCopy content =
  case content of
    Structure _ ->
        True

    Var _ _ _ ->
        True

    Alias _ _ _ ->
        True

    Error _ ->
        False



-- RESTORE


restore :: Int -> Variable -> Solver Variable
restore alreadyCopiedMark variable =
  do  desc <- liftIO $ UF.descriptor variable
      if _mark desc /= alreadyCopiedMark
        then
          return variable

        else
          do  restoredContent <-
                  restoreContent alreadyCopiedMark (_content desc)
              liftIO $ UF.setDescriptor variable $ Descriptor
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
        Structure <$> traverseFlatType go term

    Var _ _ _ ->
        return content

    Alias name args var ->
        Alias name
          <$> mapM (traverse go) args
          <*> go var

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
