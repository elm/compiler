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
  arbitrary = V.Raw <$> capVar
  shrink (V.Raw v) = V.Raw <$> shrink v

instance Arbitrary L.Literal where
  arbitrary =
      oneof
      [ L.IntNum   <$> arbitrary
      , L.FloatNum <$> (arbitrary `suchThat` noE)
      , L.Chr      <$> arbitrary
      -- This is too permissive
      , L.Str      <$> arbitrary
      -- Booleans aren't actually source syntax 
      -- , Boolean  <$> arbitrary
      ]

  shrink lit =
    case lit of
      L.IntNum n   -> L.IntNum   <$> shrink n
      L.FloatNum f -> L.FloatNum <$> (filter noE . shrink $ f)
      L.Chr c      -> L.Chr      <$> shrink c
      L.Str s      -> L.Str      <$> shrink s
      L.Boolean b  -> L.Boolean  <$> shrink b

noE :: Double -> Bool
noE = notElem 'e' . show

genVector :: Int -> (Int -> Gen a) -> Gen [a]
genVector n generator = do
  len <- choose (0,n)
  let m = n `div` (len + 1)
  vectorOf len $ generator m

instance Arbitrary v => Arbitrary (P.Pattern v) where
  arbitrary = sized pat
    where
      pat :: (Arbitrary v) => Int -> Gen (P.Pattern v)
      pat n =
          oneof
          [ pure P.Anything
          , P.Var     <$> lowVar
          , P.Record  <$> (listOf1 lowVar)
          , P.Literal <$> arbitrary
          , P.Alias   <$> lowVar <*> pat (n-1)
          , P.Data    <$> arbitrary <*> genVector n pat
          ]

  shrink pat =
    case pat of
      P.Anything  -> []
      P.Var v     -> P.Var <$> shrinkWHead v
      P.Literal l -> P.Literal <$> shrink l
      P.Alias s p -> p : (P.Alias <$> shrinkWHead s <*> shrink p)
      P.Data s ps -> ps ++ (P.Data <$> shrink s <*> shrink ps)
      P.Record fs ->
          P.Record <$> filter (all notNull) (filter notNull (shrink fs))
          where
            notNull = not . null

shrinkWHead :: Arbitrary a => [a] -> [[a]]
shrinkWHead [] = error "Should be nonempty"
shrinkWHead (x:xs) = (x:) <$> shrink xs

instance Arbitrary v => Arbitrary (T.Type v) where
  arbitrary = sized tipe
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
              , T.Var    <$> lowVar
              , T.Type   <$> arbitrary
              , T.App    <$> (T.Type <$> arbitrary) <*> depthTipes
              , T.Record <$> fields <*> pure Nothing
              , T.Record <$> fields1 <*> (Just . T.Var <$> lowVar)
              ]

  shrink tipe =
    case tipe of
      T.Lambda s t  -> s : t : (T.Lambda <$> shrink s <*> shrink t)
      T.Var _       -> []
      T.Aliased v t -> t : (T.Aliased v <$> shrink t)
      T.Type v      -> T.Type <$> shrink v
      T.App t ts    -> t : ts ++ (T.App <$> shrink t <*> shrink ts)
      T.Record fs t -> map snd fs ++ record
          where
            record =
                case t of
                  Nothing -> T.Record <$> shrinkList shrinkField fs <*> pure Nothing
                  Just _ ->
                      do fields <- filter (not . null) $ shrinkList shrinkField fs
                         return $ T.Record fields t

            shrinkField (n,t) = (,) <$> shrinkWHead n <*> shrink t

lowVar :: Gen String
lowVar = notReserved $ (:) <$> lower <*> listOf varLetter
  where
    lower = elements ['a'..'z']

capVar :: Gen String
capVar = notReserved $ (:) <$> upper <*> listOf varLetter
  where
    upper = elements ['A'..'Z']

varLetter :: Gen Char
varLetter = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\'', '_']

notReserved :: Gen String -> Gen String
notReserved = flip exceptFor Parse.Helpers.reserveds

exceptFor :: (Ord a) => Gen a -> [a] -> Gen a
exceptFor g xs = g `suchThat` notAnX
  where
    notAnX = flip Set.notMember xset
    xset = Set.fromList xs
