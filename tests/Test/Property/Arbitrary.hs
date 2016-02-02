{-# OPTIONS_GHC -W -fno-warn-orphans #-}
module Test.Property.Arbitrary where

import Control.Applicative       ((<$>), (<*>), pure)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.Set as Set
import qualified Parse.Helpers (reserveds)

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as V


instance Arbitrary V.Raw where
  arbitrary =
      V.Raw <$> capVar

  shrink (V.Raw v) =
      V.Raw <$> shrink v


instance Arbitrary L.Literal where
  arbitrary =
      oneof
        [ L.IntNum <$> arbitrary
        , L.FloatNum <$> (arbitrary `suchThat` noE)
        , L.Chr <$> arbitrary
        -- This is too permissive
        , L.Str <$> arbitrary
        -- Booleans aren't actually source syntax
        -- , Boolean  <$> arbitrary
        ]

  shrink literal =
      case literal of
        L.IntNum int ->
            L.IntNum <$> shrink int

        L.FloatNum float ->
            L.FloatNum <$> filter noE (shrink float)

        L.Chr char ->
            L.Chr <$> shrink char

        L.Str string ->
            L.Str <$> shrink string

        L.Boolean bool ->
            L.Boolean <$> shrink bool


noE :: Double -> Bool
noE =
  notElem 'e' . show


genVector :: Int -> (Int -> Gen a) -> Gen [a]
genVector n generator =
  do  len <- choose (0,n)
      let m = n `div` (len + 1)
      vectorOf len $ generator m

instance Arbitrary v => Arbitrary (P.Pattern v) where
  arbitrary =
      sized pattern
    where
      pattern :: (Arbitrary v) => Int -> Gen (P.Pattern v)
      pattern n =
          oneof
          [ pure P.Anything
          , P.Var     <$> lowVar
          , P.Record  <$> (listOf1 lowVar)
          , P.Literal <$> arbitrary
          , P.Alias   <$> lowVar <*> pattern (n-1)
          , P.Data    <$> arbitrary <*> genVector n pattern
          ]

  shrink pattern =
    case pattern of
      P.Anything ->
          []

      P.Var var ->
          P.Var <$> consAndShrink var

      P.Literal literal ->
          P.Literal <$> shrink literal

      P.Alias alias pattern ->
          pattern : (P.Alias <$> consAndShrink alias <*> shrink pattern)

      P.Data name patterns ->
          patterns ++ (P.Data <$> shrink name <*> shrink patterns)

      P.Record fields ->
          P.Record <$> filter (all notNull) (filter notNull (shrink fields))
        where
          notNull = not . null


consAndShrink :: Arbitrary a => [a] -> [[a]]
consAndShrink values =
    case values of
      [] -> error "Should be nonempty"
      x:xs -> (x:) <$> shrink xs


instance Arbitrary v => Arbitrary (T.Type v) where
  arbitrary =
      sized tipe
    where
      tipe :: Arbitrary v => Int -> Gen (T.Type v)
      tipe n =
          let depthTipe = tipe =<< choose (0,n)
              field = (,) <$> lowVar <*> depthTipe
              fields = genVector n (\m -> (,) <$> lowVar <*> tipe m)
              fields1 = (:) <$> field <*> fields
              depthTipes = (:) <$> depthTipe <*> genVector n tipe
          in
              oneof
                [ T.Lambda <$> depthTipe <*> depthTipe
                , T.Var <$> lowVar
                , T.Type <$> arbitrary
                , T.App <$> (T.Type <$> arbitrary) <*> depthTipes
                , T.Record <$> fields <*> pure Nothing
                , T.Record <$> fields1 <*> (Just . T.Var <$> lowVar)
                ]

  shrink tipe =
    case tipe of
      T.Lambda arg result ->
          arg : result : (T.Lambda <$> shrink arg <*> shrink result)

      T.Var _ ->
          []

      T.Aliased name tvars rootType ->
          rootType : (T.Aliased name <$> shrinkList shrinkPair tvars <*> shrink rootType)

      T.Type name ->
          T.Type <$> shrink name

      T.App func args ->
          func : args ++ (T.App <$> shrink func <*> shrink args)

      T.Record fields extension ->
          map snd fields ++ record
        where
          record =
              case extension of
                Nothing ->
                    T.Record <$> shrinkList shrinkPair fields <*> pure Nothing

                Just _ ->
                    do fields' <- filter (not . null) $ shrinkList shrinkPair fields
                       return $ T.Record fields' extension
    where
      shrinkPair (name, tipe) =
          (,) <$> consAndShrink name <*> shrink tipe


lowVar :: Gen String
lowVar =
    notReserved $
        (:) <$> elements ['a'..'z'] <*> listOf varLetter


capVar :: Gen String
capVar =
    notReserved $
        (:) <$> elements ['A'..'Z'] <*> listOf varLetter


varLetter :: Gen Char
varLetter =
    elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\'', '_']


notReserved :: Gen String -> Gen String
notReserved string =
    string `exceptFor` Parse.Helpers.reserveds


exceptFor :: (Ord a) => Gen a -> [a] -> Gen a
exceptFor generator list =
    generator `suchThat` notInList
  where
    set = Set.fromList list
    notInList x = Set.notMember x set
