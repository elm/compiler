module Type.Unify (unify) where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Text.PrettyPrint (render)

import qualified AST.Type as AstType
import qualified AST.Variable as Var
import qualified Reporting.Region as R
import qualified Reporting.Error.Type as Error
import qualified Type.State as TS
import Type.Type as Type
import Type.PrettyPrint
import Elm.Utils ((|>))


unify
    :: Error.Hint
    -> R.Region
    -> Variable
    -> Variable
    -> StateT TS.SolverState IO ()
unify hint region variable1 variable2 =
  do  result <- runExceptT (unifyHelp variable1 variable2)
      case result of
        Right state ->
            return state

        Left (RawMismatch (leftType, rightType, note)) ->
            TS.addError region $ Error.Mismatch $
                Error.MismatchInfo hint leftType rightType note


-- ACTUALLY UNIFY STUFF

type Unify =
  ExceptT RawMismatch (StateT TS.SolverState IO)


newtype RawMismatch =
  RawMismatch (AstType.Canonical, AstType.Canonical, Maybe String)


typeError
    :: UF.Point Descriptor
    -> UF.Point Descriptor
    -> Maybe String
    -> Unify a
typeError leftType rightType note =
  do  leftSrcType <- liftIO (toSrcType leftType)
      rightSrcType <- liftIO (toSrcType rightType)
      liftIO $ do
          UF.setDescriptor leftType (Type.descriptor Error)
          UF.setDescriptor rightType (Type.descriptor Error)
      throwError (RawMismatch (leftSrcType, rightSrcType, note))


unifyHelp :: Variable -> Variable -> Unify ()
unifyHelp variable1 variable2 =
  do  equivalent <- liftIO $ UF.equivalent variable1 variable2
      if equivalent
          then return ()
          else actuallyUnify variable1 variable2


actuallyUnify :: Variable -> Variable -> Unify ()
actuallyUnify variable1 variable2 = do
  desc1 <- liftIO $ UF.descriptor variable1
  desc2 <- liftIO $ UF.descriptor variable2
  let (name', flex', rank', alias') = combinedDescriptors desc1 desc2

      merge1 :: Unify ()
      merge1 =
        liftIO $ do
            if rank desc1 < rank desc2
                then UF.union variable2 variable1
                else UF.union variable1 variable2
            UF.modifyDescriptor variable1 $ \desc ->
                desc { structure = structure desc1
                     , flex = flex'
                     , name = name'
                     , alias = alias'
                     }

      merge2 :: Unify ()
      merge2 =
        liftIO $ do
            if rank desc1 < rank desc2
                then UF.union variable2 variable1
                else UF.union variable1 variable2
            UF.modifyDescriptor variable2 $ \desc ->
                desc { structure = structure desc2
                     , flex = flex'
                     , name = name'
                     , alias = alias'
                     }

      merge =
        if rank desc1 < rank desc2 then merge1 else merge2

      fresh :: Maybe (Term1 Variable) -> Unify Variable
      fresh structure =
        do  v <- liftIO . UF.fresh $ Descriptor
                 { structure = structure
                 , rank = rank'
                 , flex = flex'
                 , name = name'
                 , copy = Nothing
                 , mark = noMark
                 , alias = alias'
                 }
            lift (TS.register v)

      flexAndUnify v =
        do  liftIO $ UF.modifyDescriptor v $ \desc -> desc { flex = Flexible }
            unifyHelp variable1 variable2

      unifyNumber svar (Var.Canonical home name) =
          case home of
            Var.BuiltIn | name `elem` ["Int","Float"] ->
                flexAndUnify svar

            Var.Local | List.isPrefixOf "number" name ->
                flexAndUnify svar

            Var.TopLevel _ | List.isPrefixOf "number" name ->
                flexAndUnify svar
                
            _ ->
                typeError variable1 variable2 Nothing

      comparableError maybeHint =
          typeError variable1 variable2 $ Just $
            "Note: comparable types include Int, Float, Char, String, lists, and tuples."
            ++ maybe "" ("\n"++) maybeHint

      appendableError =
          typeError variable1 variable2 $ Just $
            "Note: appendable types include only Strings, Lists, and Text."

      unifyComparable v (Var.Canonical home name) =
          case home of
            Var.BuiltIn | name `elem` ["Int","Float","Char","String"] ->
                flexAndUnify v
            Var.Local | List.isPrefixOf "comparable" name ->
                flexAndUnify v
            _ ->
                comparableError Nothing

      unifyComparableStructure varSuper varFlex =
          do  struct <- liftIO $ collectApps varFlex
              case struct of
                Other ->
                    comparableError Nothing

                List v ->
                    do  flexAndUnify varSuper
                        unifyHelp v =<< liftIO (variable $ Is Comparable)

                Tuple vs ->
                    if length vs > 6 then
                      comparableError $ Just $
                        "Furthermore, it cannot be a tuple with more than 6 elements."
                    else
                      do  flexAndUnify varSuper
                          cmpVars <- liftIO $ forM [1..length vs] $ \_ -> variable (Is Comparable)
                          zipWithM_ unifyHelp vs cmpVars

      unifyAppendable varSuper varFlex =
          do  struct <- liftIO $ collectApps varFlex
              case struct of
                List _ -> flexAndUnify varSuper
                _      -> appendableError

      rigidError var =
          typeError variable1 variable2 $ Just $
            rigidErrorMessage (render (pretty Never var))

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
            _ -> typeError variable1 variable2 Nothing

  case (structure desc1, structure desc2) of
    (Nothing, Nothing)
        | flex desc1 == Flexible && flex desc1 == Flexible ->
            merge

    (Nothing, _)
        | flex desc1 == Flexible -> merge2
        | flex desc1 == Error -> return ()

    (_, Nothing)
        | flex desc2 == Flexible -> merge1
        | flex desc2 == Error -> return ()

    (Just (Var1 v), _) ->
        unifyHelp v variable2

    (_, Just (Var1 v)) ->
        unifyHelp variable1 v

    (Nothing, _) ->
        superUnify

    (_, Nothing) ->
        superUnify

    (Just type1, Just type2) ->
        case (type1,type2) of
          (App1 term1 term2, App1 term1' term2') ->
              do merge
                 unifyHelp term1 term1'
                 unifyHelp term2 term2'
          (Fun1 term1 term2, Fun1 term1' term2') ->
              do merge
                 unifyHelp term1 term1'
                 unifyHelp term2 term2'

          (EmptyRecord1, EmptyRecord1) ->
              return ()

          (Record1 fields ext, EmptyRecord1) | Map.null fields -> unifyHelp ext variable2
          (EmptyRecord1, Record1 fields ext) | Map.null fields -> unifyHelp variable1 ext

          (Record1 _ _, Record1 _ _) ->
              recordUnify fresh variable1 variable2

          _ -> typeError variable1 variable2 Nothing


-- RIGID ERROR

rigidErrorMessage :: String -> String
rigidErrorMessage var =
  "Could not unify user provided type variable `" ++ var ++ "`. The most likely cases are:\n\n"
  ++ "  1. The type you wrote down is too general. It says any type can go through\n"
  ++ "     but in fact only a certain type can.\n"
  ++ "  2. A type variable is probably being shared between two type annotations.\n"
  ++ "     They are treated as different things in the current implementation, so\n"
  ++ "     Try commenting out some type annotations and see what happens."


-- RECORD UNIFICATION

recordUnify
    :: (Maybe (Term1 Variable) -> Unify Variable)
    -> Variable
    -> Variable
    -> Unify ()
recordUnify fresh variable1 variable2 =
  do  (ExpandedRecord fields1 ext1) <- liftIO (gatherFields variable1)
      (ExpandedRecord fields2 ext2) <- liftIO (gatherFields variable2)

      unifyOverlappingFields fields1 fields2

      let freshRecord fields ext =
            fresh (Just (Record1 fields ext))

      let uniqueFields1 = diffFields fields1 fields2
      let uniqueFields2 = diffFields fields2 fields1

      let addFieldMismatchError missingFields =
              typeError variable1 variable2 $ Just $
                  fieldMismatchError missingFields

      case (ext1, ext2) of
        (Empty _, Empty _) ->
            case Map.null uniqueFields1 && Map.null uniqueFields2 of
              True -> return ()
              False -> typeError variable1 variable2 Nothing

        (Empty var1, Extension var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (_, False) -> addFieldMismatchError uniqueFields2
              (True, True) -> unifyHelp var1 var2
              (False, True) ->
                do  subRecord <- freshRecord uniqueFields1 var1
                    unifyHelp subRecord var2

        (Extension var1, Empty var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (False, _) -> addFieldMismatchError uniqueFields1
              (True, True) -> unifyHelp var1 var2
              (True, False) ->
                do  subRecord <- freshRecord uniqueFields2 var2
                    unifyHelp var1 subRecord

        (Extension var1, Extension var2) ->
            case (Map.null uniqueFields1, Map.null uniqueFields2) of
              (True, True) ->
                unifyHelp var1 var2

              (True, False) ->
                do  subRecord <- freshRecord uniqueFields2 var2
                    unifyHelp var1 subRecord

              (False, True) ->
                do  subRecord <- freshRecord uniqueFields1 var1
                    unifyHelp subRecord var2

              (False, False) ->
                do  record1' <- freshRecord uniqueFields1 =<< fresh Nothing
                    record2' <- freshRecord uniqueFields2 =<< fresh Nothing
                    unifyHelp record1' var2
                    unifyHelp var1 record2'


unifyOverlappingFields
    :: Map.Map String [Variable]
    -> Map.Map String [Variable]
    -> Unify ()
unifyOverlappingFields fields1 fields2 =
    Map.intersectionWith (zipWith unifyHelp) fields1 fields2
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
        "Looks like a record is missing the field `" ++ key ++ "`"
      keys ->
        "Looks like a record is missing fields "
        ++ List.intercalate ", " (init keys) ++ ", and " ++ last keys


combinedDescriptors
    :: Descriptor
    -> Descriptor
    -> (Maybe Var.Canonical, Flex, Int, Alias Variable)
combinedDescriptors desc1 desc2 =
    (name', flex', rank', alias')
  where
    rank' :: Int
    rank' =
      min (rank desc1) (rank desc2)

    alias' :: Alias Variable
    alias' =
      alias desc1 <|> alias desc2

    name' :: Maybe Var.Canonical
    name' =
      case (name desc1, name desc2) of
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
    flex' =
      case (flex desc1, flex desc2) of
        (f, Flexible)     -> f
        (Flexible, f)     -> f
        (Is Number, Is _) -> Is Number
        (Is _, Is Number) -> Is Number
        (Is super, Is _)  -> Is super
        (_, _)            -> Flexible
