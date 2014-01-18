{-# OPTIONS_GHC -W #-}
module Type.Unify (unify) where

import Type.Type
import qualified Data.UnionFind.IO as UF
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Type.State as TS
import Control.Monad.State
import SourceSyntax.Location
import Type.PrettyPrint
import Text.PrettyPrint (render)

unify :: SrcSpan -> Variable -> Variable -> StateT TS.SolverState IO ()
unify span variable1 variable2 = do
  equivalent <- liftIO $ UF.equivalent variable1 variable2
  if equivalent then return ()
                else actuallyUnify span variable1 variable2

actuallyUnify :: SrcSpan -> Variable -> Variable -> StateT TS.SolverState IO ()
actuallyUnify span variable1 variable2 = do
  desc1 <- liftIO $ UF.descriptor variable1
  desc2 <- liftIO $ UF.descriptor variable2
  let unify' = unify span

      name' :: Maybe String
      name' = case (name desc1, name desc2) of
                (Just name1, Just name2) ->
                    case (flex desc1, flex desc2) of
                      (_, Flexible) -> Just name1
                      (Flexible, _) -> Just name2
                      (Is Number, Is _) -> Just name1
                      (Is _, Is Number) -> Just name2
                      (Is _, Is _) -> Just name1
                      (_, _) -> Nothing
                (Just name1, _) -> Just name1
                (_, Just name2) -> Just name2
                _ -> Nothing

      flex' :: Flex
      flex' = case (flex desc1, flex desc2) of
                (f, Flexible) -> f
                (Flexible, f) -> f
                (Is Number, Is _) -> Is Number
                (Is _, Is Number) -> Is Number
                (Is super, Is _) -> Is super
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
        unify' variable1 variable2

      unifyNumber svar name
          | name `elem` ["Int","Float","number"] = flexAndUnify svar
          | otherwise = TS.addError span (Just hint) variable1 variable2
          where hint = "A number must be an Int or Float."

      comparableError maybe =
          TS.addError span (Just $ Maybe.fromMaybe msg maybe) variable1 variable2
          where msg = "A comparable must be an Int, Float, Char, String, list, or tuple."

      unifyComparable var name
          | name `elem` ["Int","Float","Char","String","comparable"] = flexAndUnify var
          | otherwise = comparableError Nothing

      unifyComparableStructure varSuper varFlex =
          do struct <- liftIO $ collectApps varFlex
             case struct of
               Other -> comparableError Nothing
               List v -> do flexAndUnify varSuper
                            unify' v =<< liftIO (var $ Is Comparable)
               Tuple vs
                   | length vs > 6 ->
                       comparableError $ Just "Cannot compare a tuple with more than 6 elements."
                   | otherwise -> 
                       do flexAndUnify varSuper
                          cmpVars <- liftIO $ forM [1..length vs] $ \_ -> var (Is Comparable)
                          zipWithM_ unify' vs cmpVars

      unifyAppendable varSuper varFlex =
          do struct <- liftIO $ collectApps varFlex
             case struct of
               List _ -> flexAndUnify varSuper
               _ -> comparableError Nothing

      rigidError variable = TS.addError span (Just hint) variable1 variable2
          where
            var = "'" ++ render (pretty Never variable) ++ "'"
            hint = "Cannot unify rigid type variable " ++ var ++
                   ".\nThe problem probably relates to a type annotation. Note that rigid type\n\
                   \variables are not shared between a top-level and let-bound type annotations."

      superUnify =
          case (flex desc1, flex desc2, name desc1, name desc2) of
            (Is super1, Is super2, _, _)
                | super1 == super2 -> merge
            (Is Number, Is Comparable, _, _) -> merge1
            (Is Comparable, Is Number, _, _) -> merge2
                   
            (Is Number, _, _, Just name) -> unifyNumber variable1 name
            (_, Is Number, Just name, _) -> unifyNumber variable2 name

            (Is Comparable, _, _, Just name) -> unifyComparable variable1 name
            (_, Is Comparable, Just name, _) -> unifyComparable variable2 name
            (Is Comparable, _, _, _) -> unifyComparableStructure variable1 variable2
            (_, Is Comparable, _, _) -> unifyComparableStructure variable2 variable1

            (Is Appendable, _, _, Just ctor)
                | ctor `elem` ["Text.Text","String"] -> flexAndUnify variable1
            (_, Is Appendable, Just ctor, _)
                | ctor `elem` ["Text.Text","String"] -> flexAndUnify variable2
            (Is Appendable, _, _, _) -> unifyAppendable variable1 variable2
            (_, Is Appendable, _, _) -> unifyAppendable variable2 variable1

            (Rigid, _, _, _) -> rigidError variable1
            (_, Rigid, _, _) -> rigidError variable2
            _ -> TS.addError span Nothing variable1 variable2

  case (structure desc1, structure desc2) of
    (Nothing, Nothing) | flex desc1 == Flexible && flex desc1 == Flexible -> merge
    (Nothing, _) | flex desc1 == Flexible -> merge2
    (_, Nothing) | flex desc2 == Flexible -> merge1

    (Just (Var1 v), _) -> unify' v variable2
    (_, Just (Var1 v)) -> unify' v variable1

    (Nothing, _) -> superUnify
    (_, Nothing) -> superUnify

    (Just type1, Just type2) ->
        case (type1,type2) of
          (App1 term1 term2, App1 term1' term2') ->
              do merge
                 unify' term1 term1'
                 unify' term2 term2'
          (Fun1 term1 term2, Fun1 term1' term2') ->
              do merge
                 unify' term1 term1'
                 unify' term2 term2'

          (EmptyRecord1, EmptyRecord1) ->
              return ()

          (Record1 fields ext, EmptyRecord1) | Map.null fields -> unify' ext variable2
          (EmptyRecord1, Record1 fields ext) | Map.null fields -> unify' ext variable1

          (Record1 fields1 ext1, Record1 fields2 ext2) ->
              do sequence . concat . Map.elems $ Map.intersectionWith (zipWith unify') fields1 fields2
                 let mkRecord fs ext = fresh . Just $ Record1 fs ext
                 case (Map.null fields1', Map.null fields2') of
                   (True , True ) -> unify' ext1 ext2
                   (True , False) -> do
                      record2' <- mkRecord fields2' ext2
                      unify' ext1 record2'
                   (False, True ) -> do
                      record1' <- mkRecord fields1' ext1
                      unify' record1' ext2
                   (False, False) -> do
                      record1' <- mkRecord fields1' =<< fresh Nothing
                      record2' <- mkRecord fields2' =<< fresh Nothing
                      unify' record1' ext2
                      unify' ext1 record2'
              where
                fields1' = unmerged fields1 fields2
                fields2' = unmerged fields2 fields1

                unmerged a b = Map.filter (not . null) $ Map.union (Map.intersectionWith eat a b) a

                eat (_:xs) (_:ys) = eat xs ys
                eat xs _ = xs

          _ -> TS.addError span Nothing variable1 variable2

