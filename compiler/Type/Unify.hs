module Type.Unify (unify) where

import Type.Type
import qualified Data.UnionFind.IO as UF
import qualified Type.Pool as Pool
import Control.Arrow (first,second)
import Control.Monad.State

unify :: Variable -> Variable -> StateT (Pool.Pool, [String]) IO ()
unify variable1 variable2 = do
  equivalent <- liftIO $ UF.equivalent variable1 variable2
  if equivalent then return ()
                else actuallyUnify variable1 variable2

actuallyUnify :: Variable -> Variable -> StateT (Pool.Pool, [String]) IO ()
actuallyUnify variable1 variable2 = do
  desc1 <- liftIO $ UF.descriptor variable1
  desc2 <- liftIO $ UF.descriptor variable2
  let name' :: Maybe String
      name' = case (name desc1, name desc2) of
                (Just name1, Just name2)
                    | name1 == name2 -> Just name1
                    | flex desc1 /= Flexible -> Just name1
                    | flex desc2 /= Flexible -> Just name2
                    | otherwise -> Nothing
                (Just name1, _) -> Just name1
                (_, Just name2) -> Just name2
                _ -> Nothing

      flex' :: Flex
      flex' = if flex desc1 /= Flexible then flex desc1 else
              if flex desc2 /= Flexible then flex desc2 else Flexible

      rank' :: Int
      rank' = min (rank desc1) (rank desc2)

      merge1 :: StateT (Pool.Pool, [String]) IO ()
      merge1 = liftIO $ do
        UF.union variable2 variable1
        UF.setDescriptor variable1 (desc1 { flex = flex', name = name', rank = rank', mark = undefined })

      merge2 :: StateT (Pool.Pool, [String]) IO ()
      merge2 = liftIO $ do
        UF.union variable1 variable2
        UF.setDescriptor variable2 (desc2 { flex = flex', name = name', rank = rank', mark = undefined })

      fresh :: Maybe (Term1 Variable) -> StateT (Pool.Pool, [String]) IO Variable
      fresh structure = do
        var <- liftIO . UF.fresh $ Descriptor {
                 structure = structure, rank = rank', flex = flex', name = name', mark = undefined
               }
        Pool.register var

      mistake :: String -> StateT (Pool.Pool, [String]) IO ()
      mistake err = modify $ second (err:)

  case (structure desc1, structure desc2) of
    (Nothing, _) | flex desc1 == Flexible -> merge2
    (_, Nothing) | flex desc2 == Flexible -> merge1

    (Just (Var1 v), _) -> unify v variable2
    (_, Just (Var1 v)) -> unify v variable1

    (Nothing, _) -> mistake "Cannot unify rigid type variable."
    (_, Nothing) -> mistake "Cannot unify rigid type variable."

    (Just type1, Just type2) ->
        case (type1,type2) of
          (App1 term1 term2, App1 term1' term2') ->
              do merge1
                 unify term1 term1'
                 unify term2 term2'
          (Fun1 term1 term2, Fun1 term1' term2') ->
              do merge1
                 unify term1 term1'
                 unify term2 term2'

          (EmptyRecord1, EmptyRecord1) ->
              return ()

          (Record1 fields1 ext1, Record1 fields2 ext2) ->
              mistake "did not write record unification yet"

          _ -> mistake "Could not unify types"