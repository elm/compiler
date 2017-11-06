{-# OPTIONS_GHC -Wall #-}
module Type.State
  ( Solver(..)
  , run
  , State(..)
  , Pools
  , Env
  , modifyEnv
  , addError
  , getEnv
  , saveLocalEnv
  , register
  , introduce
  , flatten
  , copy
  )
  where


import Control.Monad (forM_, liftM, liftM2, liftM3)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Vector.Mutable as MVector

import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type as Type
import qualified Type.UnionFind as UF



-- SOLVER


newtype Solver a =
  Solver { _solve :: State -> IO (a, State) }


data State =
  SS
    { _env :: Env
    , _savedEnv :: Env
    , _rank :: Int
    , _pools :: Pools
    , _mark :: Mark
    , _errors :: [A.Located Error.Error]
    }


type Pools = MVector.IOVector [Variable]


type Env = Map.Map N.Name (A.Located Variable)



-- RUN SOLVER


run :: Solver () -> IO State
run (Solver solver) =
  do  pools <- MVector.replicate 8 []
      (_, state) <- solver (SS Map.empty Map.empty outermostRank pools (Type.nextMark noMark) [])
      return state



-- HELPERS


{-# INLINE modifyEnv #-}
modifyEnv :: (Env -> Env) -> Solver ()
modifyEnv f =
  Solver $ \state ->
    return ( (), state { _env = f (_env state) } )


{-# INLINE addError #-}
addError :: R.Region -> Error.Error -> Solver ()
addError region err =
  Solver $ \state ->
    return ( (), state { _errors = A.A region err : _errors state } )


{-# INLINE getEnv #-}
getEnv :: Solver Env
getEnv =
  Solver $ \state ->
    return ( _env state, state )


{-# INLINE saveLocalEnv #-}
saveLocalEnv :: Solver ()
saveLocalEnv =
  Solver $ \state ->
    return ( (), state { _savedEnv = _env state } )



-- REGISTER VARIABLES


register :: Variable -> Solver ()
register var =
  Solver $ \state@(SS _ _ maxRank pools _ _) ->
    {-# SCC elm_compiler_type_register #-}
    do  MVector.modify pools (var:) maxRank
        return ( (), state )


introduce :: [Variable] -> Solver ()
introduce variables =
  Solver $ \state@(SS _ _ maxRank pools _ _) ->
    {-# SCC elm_compiler_type_introduce #-}
    do
        forM_ variables $ \var ->
          UF.modifyDescriptor var $ \(Descriptor content _ mark cp) ->
            Descriptor content maxRank mark cp

        MVector.modify pools (variables++) maxRank

        return ( (), state )



-- FLATTEN


flatten :: Type -> Solver Variable
flatten tipe =
  Solver $ \state@(SS _ _ maxRank pools _ _) ->
    {-# SCC elm_compiler_type_flatten #-}
    do  var <- flattenHelp maxRank pools Map.empty tipe
        return (var, state)


flattenHelp :: Int -> Pools -> Map.Map N.Name Variable -> Type -> IO Variable
flattenHelp maxRank pools aliasDict tipe =
  case tipe of
    PlaceHolder name ->
        return (aliasDict ! name)

    AliasN home name args realType ->
        do  flatArgs <- mapM (traverse (flattenHelp maxRank pools aliasDict)) args
            flatVar <- flattenHelp maxRank pools (Map.fromList flatArgs) realType
            registerFlatType maxRank pools (Alias home name flatArgs flatVar)

    VarN v ->
        return v

    AppN home name args ->
        do  flatArgs <- traverse (flattenHelp maxRank pools aliasDict) args
            registerFlatType maxRank pools (Structure (App1 home name flatArgs))

    FunN a b ->
        do  flatA <- flattenHelp maxRank pools aliasDict a
            flatB <- flattenHelp maxRank pools aliasDict b
            registerFlatType maxRank pools (Structure (Fun1 flatA flatB))

    EmptyRecordN ->
        registerFlatType maxRank pools (Structure EmptyRecord1)

    RecordN fields ext ->
        do  flatFields <- traverse (flattenHelp maxRank pools aliasDict) fields
            flatExt <- flattenHelp maxRank pools aliasDict ext
            registerFlatType maxRank pools (Structure (Record1 flatFields flatExt))

    UnitN ->
        registerFlatType maxRank pools (Structure Unit1)

    TupleN a b cs ->
        do  flatA <- flattenHelp maxRank pools aliasDict a
            flatB <- flattenHelp maxRank pools aliasDict b
            flatCs <- traverse (flattenHelp maxRank pools aliasDict) cs
            registerFlatType maxRank pools (Structure (Tuple1 flatA flatB flatCs))


registerFlatType :: Int -> Pools -> Content -> IO Variable
registerFlatType maxRank pools content =
  do  var <- UF.fresh (Descriptor content maxRank noMark Nothing)
      MVector.modify pools (var:) maxRank
      return var



-- COPY


copy :: Variable -> Solver Variable
copy var =
  Solver $ \state@(SS _ _ maxRank pools _ _) ->
    {-# SCC elm_compiler_type_copy #-}
    do  copyVar <- copyHelp maxRank pools var
        _ <- restore var
        return ( copyVar, state )


copyHelp :: Int -> Pools -> Variable -> IO Variable
copyHelp maxRank pools variable =
  do  (Descriptor content rank _mark maybeCopy) <- UF.descriptor variable
      case maybeCopy of
        Just copiedVariable ->
          return copiedVariable

        Nothing ->
          if rank /= noRank then
            return variable

          else
            do  let mkCopyDesc cont = Descriptor cont maxRank noMark Nothing
                newVar <- UF.fresh $ mkCopyDesc content
                MVector.modify pools (newVar:) maxRank

                -- Link the original variable to the new variable. This lets us
                -- avoid making multiple copies of the variable we are instantiating.
                --
                -- Need to do this before recursively copying the content of
                -- the variable to avoid looping on cyclic terms.
                UF.setDescriptor variable $
                  Descriptor content rank noMark (Just newVar)

                -- Now we recursively copy the content of the variable.
                -- We have already marked the variable as copied, so we
                -- will not repeat this work or crawl this variable again.
                case content of
                  Structure term ->
                    do  newTerm <- traverseFlatType (copyHelp maxRank pools) term
                        UF.setDescriptor newVar $ mkCopyDesc (Structure newTerm)

                  FlexVar _ ->
                    return ()

                  FlexSuper _ _ ->
                    return ()

                  RigidVar name ->
                    UF.setDescriptor newVar $ mkCopyDesc $ FlexVar (Just name)

                  RigidSuper super name ->
                    UF.setDescriptor newVar $ mkCopyDesc $ FlexSuper super (Just name)

                  Alias home name args realType ->
                    do  newArgs <- mapM (traverse (copyHelp maxRank pools)) args
                        newRealType <- copyHelp maxRank pools realType
                        UF.setDescriptor newVar $ mkCopyDesc (Alias home name newArgs newRealType)

                  Error _ ->
                    return ()

                return newVar



-- RESTORE


restore :: Variable -> IO Variable
restore variable =
  do  (Descriptor content _rank _mark maybeCopy) <- UF.descriptor variable
      case maybeCopy of
        Nothing ->
          return variable

        Just _ ->
          do  restoredContent <- restoreContent content
              UF.setDescriptor variable $
                Descriptor restoredContent noRank noMark Nothing
              return variable


restoreContent :: Content -> IO Content
restoreContent content =
  case content of
    FlexVar _ ->
        return content

    FlexSuper _ _ ->
        return content

    RigidVar _ ->
        return content

    RigidSuper _ _ ->
        return content

    Structure term ->
        Structure <$> traverseFlatType restore term

    Alias home name args var ->
        Alias home name
          <$> mapM (traverse restore) args
          <*> restore var

    Error _ ->
        return content



-- TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> IO Variable) -> FlatType -> IO FlatType
traverseFlatType f flatType =
  case flatType of
    App1 home name args ->
        liftM (App1 home name) (traverse f args)

    Fun1 a b ->
        liftM2 Fun1 (f a) (f b)

    EmptyRecord1 ->
        pure EmptyRecord1

    Record1 fields ext ->
        liftM2 Record1 (traverse f fields) (f ext)

    Unit1 ->
        pure Unit1

    Tuple1 a b cs ->
        liftM3 Tuple1 (f a) (f b) (traverse f cs)



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
