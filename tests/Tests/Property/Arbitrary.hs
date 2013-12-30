{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Property.Arbitrary where

import Control.Applicative       ((<$>), (<*>), pure)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.Set as Set
import qualified Parse.Helpers (reserveds)

import SourceSyntax.Literal
import SourceSyntax.Pattern

instance Arbitrary Literal where
  arbitrary = oneof [ IntNum   <$> arbitrary
                    , FloatNum <$> (arbitrary `suchThat` noE)
                    , Chr      <$> arbitrary
                      -- This is too permissive
                    , Str      <$> arbitrary
                      -- Booleans aren't actually source syntax 
                      -- , Boolean  <$> arbitrary
                    ]
  shrink l = case l of
    IntNum n   -> IntNum   <$> shrink n
    FloatNum f -> FloatNum <$> (filter noE . shrink $ f)
    Chr c      -> Chr      <$> shrink c
    Str s      -> Str      <$> shrink s
    Boolean b  -> Boolean  <$> shrink b

noE :: Double -> Bool
noE = notElem 'e' . show


instance Arbitrary Pattern where
  arbitrary = sized pat
    where pat :: Int -> Gen Pattern
          pat n = oneof [ pure PAnything
                        , PVar     <$> lowVar
                        , PRecord  <$> (listOf1 lowVar)
                        , PLiteral <$> arbitrary
                        , PAlias   <$> lowVar    <*> pat (n-1)
                        , PData    <$> capVar    <*> sizedPats
                        ]
            where sizedPats = do
                    len <- choose (0,n)
                    let m = n `div` len in
                      vectorOf len $ pat m

  shrink pat = case pat of
    PAnything  -> []
    PVar v     -> PVar     <$> shrinkWHead v
    PRecord fs -> PRecord  <$> (filter (all $ not . null) . filter (not . null) $ shrink fs)
    PLiteral l -> PLiteral <$> shrink l
    PAlias s p -> p : (PAlias <$> shrinkWHead s <*> shrink p)
    PData s ps -> ps ++ (PData <$> shrinkWHead s <*> shrink ps)
    where shrinkWHead (x:xs) = (x:) <$> shrink xs

lowVar :: Gen String
lowVar = notReserved $ (:) <$> lower <*> listOf varLetter
  where lower = elements ['a'..'z']

capVar :: Gen String
capVar = notReserved $ (:) <$> upper <*> listOf varLetter
  where upper = elements ['A'..'Z']

varLetter :: Gen Char
varLetter = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\'', '_']

notReserved :: Gen String -> Gen String
notReserved = flip exceptFor Parse.Helpers.reserveds

exceptFor :: (Ord a) => Gen a -> [a] -> Gen a
exceptFor g xs = g `suchThat` notAnX
  where notAnX = flip Set.notMember xset
        xset = Set.fromList xs
