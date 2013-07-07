module Type.Unify (unify) where

import Type.Type
import qualified Data.UnionFind.IO as UF

success = return []

unify register variable1 variable2 =
    do equivalent <- UF.equivalent variable1 variable2
       if equivalent then success else do
         desc1 <- UF.descriptor variable1
         desc2 <- UF.descriptor variable2
         let name' = case (name desc1, name desc2) of
                       (Just name1, Just name2)
                           | name1 == name2 -> Just name1
                           | flex desc1 /= Flexible -> Just name1
                           | flex desc2 /= Flexible -> Just name2
                           | otherwise -> Nothing
                       (Just name1, _) -> Just name1
                       (_, Just name2) -> Just name2
                       _ -> Nothing

             flex' = if flex desc1 /= Flexible then flex desc1 else
                     if flex desc2 /= Flexible then flex desc2 else Flexible

             rank' = min (rank desc1) (rank desc2)

             merge1 = do
               UF.union variable2 variable1
               UF.setDescriptor variable1 (desc1 { flex = flex', name = name', rank = rank', mark = undefined })

             merge2 = do
               UF.union variable1 variable2
               UF.setDescriptor variable2 (desc2 { flex = flex', name = name', rank = rank', mark = undefined })

             fresh structure = -- gotta register this
               UF.fresh $ Descriptor {
                 structure = structure, rank = rank', flex = flex', name = name', mark = undefined
               }

         case (structure desc1, structure desc2) of
           (Nothing, _) | flex desc1 == Flexible -> merge2 >> success
           (_, Nothing) | flex desc2 == Flexible -> merge1 >> success
           (Just (Var1 v), _) -> unify register v variable2
           (_, Just (Var1 v)) -> unify register v variable1
           (Nothing, _) -> return ["Cannot unify rigid type variable."]
           (_, Nothing) -> return ["Cannot unify rigid type variable."]
           (Just type1, Just type2) ->
               case (type1,type2) of
                 (App1 term1 term2, App1 term1' term2') ->
                     do merge1
                        ok1 <- unify register term1 term1'
                        ok2 <- unify register term2 term2'
                        return (ok1 ++ ok2)
                 (Fun1 term1 term2, Fun1 term1' term2') ->
                     do merge1
                        ok1 <- unify register term1 term1'
                        ok2 <- unify register term2 term2'
                        return (ok1 ++ ok2)
           