module Type.Unify (unify) where

import Control.Applicative ((<|>))
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.UnionFind.IO as UF
import qualified AST.Annotation as A
import qualified AST.Variable as Var
import qualified Type.State as TS
import Type.Type
import Type.PrettyPrint
import Text.PrettyPrint (render)
import Elm.Utils ((|>))


unify :: A.Region -> Variable -> Variable -> StateT TS.SolverState IO ()
unify region variable1 variable2 = do
  equivalent <- liftIO $ UF.equivalent variable1 variable2
  if equivalent
      then return ()
      else actuallyUnify region variable1 variable2


actuallyUnify :: A.Region -> Variable -> Variable -> StateT TS.SolverState IO ()
actuallyUnify region variable1 variable2 = do
  desc1 <- liftIO $ UF.descriptor variable1
  desc2 <- liftIO $ UF.descriptor variable2
  let unify' = unify region

      (name', flex', rank', alias') = combinedDescriptors desc1 desc2

      merge1 :: StateT TS.SolverState IO ()
      merge1 = liftIO $ do
        if rank desc1 < rank desc2 then UF.union variable2 variable1
                                   else UF.union variable1 variable2
        UF.modifyDescriptor variable1 $ \desc ->
            desc { structure = structure desc1
                 , flex = flex'
                 , name = name'
                 , alias = alias'
                 }

      merge2 :: StateT TS.SolverState IO ()
      merge2 = liftIO $ do
        if rank desc1 < rank desc2 then UF.union variable2 variable1
                                   else UF.union variable1 variable2
        UF.modifyDescriptor variable2 $ \desc ->
            desc { structure = structure desc2
                 , flex = flex'
                 , name = name'
                 , alias = alias'
                 }

      merge = if rank desc1 < rank desc2 then merge1 else merge2

      fresh :: Maybe (Term1 Variable) -> StateT TS.SolverState IO Variable
      fresh structure = do
        v <- liftIO . UF.fresh $ Descriptor
             { structure = structure
             , rank = rank'
             , flex = flex'
             , name = name'
             , copy = Nothing
             , mark = noMark
             , alias = alias'
             }
        TS.register v

      flexAndUnify v = do
        liftIO $ UF.modifyDescriptor v $ \desc -> desc { flex = Flexible }
        unify' variable1 variable2

      unifyNumber svar (Var.Canonical home name) =
          case home of
            Var.BuiltIn | name `elem` ["Int","Float"]   -> flexAndUnify svar
            Var.Local   | List.isPrefixOf "number" name -> flexAndUnify svar
            _ ->
              let hint = "Looks like something besides an Int or Float is being used as a number."
              in
                  TS.addError region (Just hint) variable1 variable2

      comparableError maybe =
          TS.addError region (Just $ Maybe.fromMaybe msg maybe) variable1 variable2
        where
          msg =
            "Looks like you want something comparable, but the only valid comparable\n\
            \types are Int, Float, Char, String, lists, or tuples."

      appendableError maybe =
          TS.addError region (Just $ Maybe.fromMaybe msg maybe) variable1 variable2
        where
          msg =
            "Looks like you want something appendable, but the only Strings, Lists,\n\
            \and Text can be appended with the (++) operator."

      unifyComparable v (Var.Canonical home name) =
          case home of
            Var.BuiltIn | name `elem` ["Int","Float","Char","String"] -> flexAndUnify v
            Var.Local   | List.isPrefixOf "comparable" name           -> flexAndUnify v
            _ -> comparableError Nothing

      unifyComparableStructure varSuper varFlex =
          do struct <- liftIO $ collectApps varFlex
             case struct of
               Other -> comparableError Nothing
               List v -> do flexAndUnify varSuper
                            unify' v =<< liftIO (variable $ Is Comparable)
               Tuple vs
                   | length vs > 6 ->
                       comparableError $ Just "Cannot compare a tuple with more than 6 elements."
                   | otherwise -> 
                       do flexAndUnify varSuper
                          cmpVars <- liftIO $ forM [1..length vs] $ \_ -> variable (Is Comparable)
                          zipWithM_ unify' vs cmpVars

      unifyAppendable varSuper varFlex =
          do struct <- liftIO $ collectApps varFlex
             case struct of
               List _ -> flexAndUnify varSuper
               _      -> appendableError Nothing

      rigidError var =
          TS.addError region (Just hint) variable1 variable2
        where
          hint =
            "Could not unify rigid type variable '" ++ render (pretty Never var) ++ "'.\n" ++
            "The problem probably relates to the type variable being shared between a\n\
            \top-level type annotation and a related let-bound type annotation."

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

            (Is Appendable, _, _, Just name)
                | Var.isText name || Var.isPrim "String" name -> flexAndUnify variable1
            (_, Is Appendable, Just name, _)
                | Var.isText name || Var.isPrim "String" name -> flexAndUnify variable2
            (Is Appendable, _, _, _) -> unifyAppendable variable1 variable2
            (_, Is Appendable, _, _) -> unifyAppendable variable2 variable1

            (Rigid, _, _, _) -> rigidError variable1
            (_, Rigid, _, _) -> rigidError variable2
            _ -> TS.addError region Nothing variable1 variable2

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

          (Record1 _ _, Record1 _ _) ->
              recordUnify region fresh variable1 variable2

          _ -> TS.addError region Nothing variable1 variable2


-- RECORD UNIFICATION

recordUnify
    :: A.Region
    -> (Maybe (Term1 Variable) -> StateT TS.SolverState IO Variable)
    -> Variable
    -> Variable
    -> StateT TS.SolverState IO ()
recordUnify region fresh variable1 variable2 =
  do  (ExpandedRecord fields1 ext1) <- liftIO (gatherFields variable1)
      (ExpandedRecord fields2 ext2) <- liftIO (gatherFields variable2)

      unifyOverlappingFields region fields1 fields2

      let freshRecord fields ext =
            fresh (Just (Record1 fields ext))

      let uniqueFields1 = diffFields fields1 fields2
      let uniqueFields2 = diffFields fields2 fields1

      let addFieldMismatchError missingFields =
            let msg = fieldMismatchError missingFields
            in
                TS.addError region (Just msg) variable1 variable2

      case (ext1, ext2) of
        (Empty _, Empty _) ->
            case Map.null uniqueFields1 && Map.null uniqueFields2 of
              True -> return ()
              False -> TS.addError region Nothing variable1 variable2

        (Empty var1, Extension var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (_, False) -> addFieldMismatchError uniqueFields2
              (True, True) -> unify region var1 var2
              (False, True) ->
                do  subRecord <- freshRecord uniqueFields1 var1
                    unify region subRecord var2

        (Extension var1, Empty var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (False, _) -> addFieldMismatchError uniqueFields1
              (True, True) -> unify region var1 var2
              (True, False) ->
                do  subRecord <- freshRecord uniqueFields2 var2
                    unify region var1 subRecord

        (Extension var1, Extension var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (True, True) ->
                unify region var1 var2

              (True, False) ->
                do  subRecord <- freshRecord uniqueFields2 var2
                    unify region var1 subRecord

              (False, True) ->
                do  subRecord <- freshRecord uniqueFields1 var1
                    unify region subRecord var2

              (False, False) ->
                do  record1' <- freshRecord uniqueFields1 =<< fresh Nothing
                    record2' <- freshRecord uniqueFields2 =<< fresh Nothing
                    unify region record1' var2
                    unify region var1 record2'


unifyOverlappingFields
    :: A.Region
    -> Map.Map String [Variable]
    -> Map.Map String [Variable]
    -> StateT TS.SolverState IO ()
unifyOverlappingFields region fields1 fields2 =
    Map.intersectionWith (zipWith (unify region)) fields1 fields2
        |> Map.elems 
        |> concat
        |> sequence_


diffFields :: Map.Map String [a] -> Map.Map String [a] -> Map.Map String [a]
diffFields fields1 fields2 =
  let eat (_:xs) (_:ys) = eat xs ys
      eat xs _ = xs
  in
      Map.union (Map.intersectionWith eat fields1 fields2) fields1
        |> Map.filter (not . null)


data ExpandedRecord = ExpandedRecord
    { _fields :: Map.Map String [Variable]
    , _extension :: Extension
    }

data Extension = Empty Variable | Extension Variable


gatherFields :: Variable -> IO ExpandedRecord
gatherFields var =
  do  desc <- UF.descriptor var
      case structure desc of
        (Just (Record1 fields ext)) ->
          do  (ExpandedRecord deeperFields rootExt) <- gatherFields ext
              return (ExpandedRecord (Map.unionWith (++) fields deeperFields) rootExt)

        (Just EmptyRecord1) ->
          return (ExpandedRecord Map.empty (Empty var))

        _ ->
          return (ExpandedRecord Map.empty (Extension var))


-- assumes that one of the dicts has stuff in it
fieldMismatchError :: Map.Map String a -> String
fieldMismatchError missingFields =
    case Map.keys missingFields of
      [] -> ""
      [key] ->
        "Looks like a record is missing the field '" ++ key ++ "'.\n    " ++
        "Maybe there is a misspelling in a record access or record update?"
      keys ->
        "Looks like one record is missing fields "
        ++ List.intercalate ", " (init keys) ++ ", and " ++ last keys


combinedDescriptors :: Descriptor -> Descriptor
                    -> (Maybe Var.Canonical, Flex, Int, Maybe Var.Canonical)
combinedDescriptors desc1 desc2 =
    (name', flex', rank', alias')
  where
    rank' :: Int
    rank' = min (rank desc1) (rank desc2)

    alias' :: Maybe Var.Canonical
    alias' = alias desc1 <|> alias desc2

    name' :: Maybe Var.Canonical
    name' = case (name desc1, name desc2) of
              (Just name1, Just name2) ->
                  case (flex desc1, flex desc2) of
                    (_, Flexible)     -> Just name1
                    (Flexible, _)     -> Just name2
                    (Is Number, Is _) -> Just name1
                    (Is _, Is Number) -> Just name2
                    (Is _, Is _)      -> Just name1
                    (_, _)            -> Nothing
              (Just name1, _) -> Just name1
              (_, Just name2) -> Just name2
              _ -> Nothing

    flex' :: Flex
    flex' = case (flex desc1, flex desc2) of
              (f, Flexible)     -> f
              (Flexible, f)     -> f
              (Is Number, Is _) -> Is Number
              (Is _, Is Number) -> Is Number
              (Is super, Is _)  -> Is super
              (_, _)            -> Flexible
