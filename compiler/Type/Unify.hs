module Type.Unify (unify) where

import Type.Type
import qualified Data.UnionFind.IO as UF
import qualified Type.State as TS
import Control.Arrow (first,second)
import Control.Monad.State
import qualified Text.PrettyPrint as P

unify :: Variable -> Variable -> StateT TS.SolverState IO ()
unify variable1 variable2 = do
  equivalent <- liftIO $ UF.equivalent variable1 variable2
  if equivalent then return ()
                else actuallyUnify variable1 variable2

actuallyUnify :: Variable -> Variable -> StateT TS.SolverState IO ()
actuallyUnify variable1 variable2 = do
  desc1 <- liftIO $ UF.descriptor variable1
  desc2 <- liftIO $ UF.descriptor variable2
  let name' :: Maybe String
      name' = case (name desc1, name desc2) of
                (Just name1, Just name2) ->
                    case (flex desc1, flex desc2) of
                      (_, Flexible) -> Just name1
                      (Flexible, _) -> Just name2
                      (IsIn Number, IsIn _) -> Just name1
                      (IsIn _, IsIn Number) -> Just name2
                      (IsIn _, IsIn _) -> Just name1
                      (_, _) -> Nothing
                (Just name1, _) -> Just name1
                (_, Just name2) -> Just name2
                _ -> Nothing

      flex' :: Flex
      flex' = case (flex desc1, flex desc2) of
                (f, Flexible) -> f
                (Flexible, f) -> f
                (IsIn Number, IsIn _) -> IsIn Number
                (IsIn _, IsIn Number) -> IsIn Number
                (IsIn super, IsIn _) -> IsIn super
                (_, _) -> Flexible

      rank' :: Int
      rank' = min (rank desc1) (rank desc2)

      merge1 :: StateT TS.SolverState IO ()
      merge1 = liftIO $ do
        if rank desc1 < rank desc2 then UF.union variable2 variable1
                                   else UF.union variable1 variable2
        UF.modifyDescriptor variable1 $ \desc ->
            desc { structure = structure desc1, flex = flex', name = name' }

      merge2 :: StateT TS.SolverState IO ()
      merge2 = liftIO $ do
        if rank desc1 < rank desc2 then UF.union variable2 variable1
                                   else UF.union variable1 variable2
        UF.modifyDescriptor variable2 $ \desc ->
            desc { structure = structure desc2, flex = flex', name = name' }

      merge = if rank desc1 < rank desc2 then merge1 else merge2

      fresh :: Maybe (Term1 Variable) -> StateT TS.SolverState IO Variable
      fresh structure = do
        var <- liftIO . UF.fresh $ Descriptor {
                 structure = structure, rank = rank', flex = flex',
                 name = name', copy = Nothing, mark = noMark
               }
        TS.register var

      flexAndUnify var = do
        liftIO $ UF.modifyDescriptor var $ \desc -> desc { flex = Flexible }
        unify variable1 variable2

      superUnify =
          case (flex desc1, flex desc2, name desc1, name desc2) of
            (IsIn super1, IsIn super2, _, _)
                | super1 == super2 -> merge
            (IsIn Number, IsIn Comparable, _, _) -> merge1
            (IsIn Comparable, IsIn Number, _, _) -> merge2
                   
            (IsIn Number, _, _, Just name)
                | name `elem` ["Int","Float"] -> flexAndUnify variable1
                | otherwise -> TS.addError "Expecting a number (Int or Float)" variable1 variable2

            (_, IsIn Number, Just name, _)
                | name `elem` ["Int","Float"] -> flexAndUnify variable2
                | otherwise -> TS.addError "Expecting a number (Int or Float)" variable1 variable2

            (IsIn Comparable, _, _, Just name)
                | name `elem` ["Int","Float","Char"] -> flexAndUnify variable1
                | otherwise -> TS.addError "Expecting something comparable (Int, Float, Char, [comparable])." variable1 variable2

            (_, IsIn Comparable, Just name, _)
                | name `elem` ["Int","Float","Char"] -> flexAndUnify variable2
                | otherwise -> TS.addError "Expecting something comparable (Int, Float, Char, [comparable])." variable1 variable2

            _ -> TS.addError "The following types are not equal" variable1 variable2

  case (structure desc1, structure desc2) of
    (Nothing, Nothing) | flex desc1 == Flexible && flex desc1 == Flexible -> merge
    (Nothing, _) | flex desc1 == Flexible -> merge2
    (_, Nothing) | flex desc2 == Flexible -> merge1

    (Just (Var1 v), _) -> unify v variable2
    (_, Just (Var1 v)) -> unify v variable1

    (Nothing, _) -> superUnify
    (_, Nothing) -> superUnify

    (Just type1, Just type2) ->
        case (type1,type2) of
          (App1 term1 term2, App1 term1' term2') ->
              do merge
                 unify term1 term1'
                 unify term2 term2'
          (Fun1 term1 term2, Fun1 term1' term2') ->
              do merge
                 unify term1 term1'
                 unify term2 term2'

          (EmptyRecord1, EmptyRecord1) ->
              return ()

          (Record1 fields1 ext1, Record1 fields2 ext2) ->
              TS.addError "did not write record unification yet" variable1 variable2

          _ -> TS.addError "could not unify types" variable1 variable2

