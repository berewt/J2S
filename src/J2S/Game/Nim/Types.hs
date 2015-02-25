{-# LANGUAGE FlexibleContexts #-}

module J2S.Game.Nim.Types
  ( ANatural
  , aNatural
  , ANonEmpty
  , aNonEmpty
  , NonEmptyHeaps
  , neh
  ) where

import Control.Monad (mfilter)
import Control.Lens

import qualified Data.Foldable as F

import qualified Test.QuickCheck as Q

import qualified Data.List.NonEmpty as NE

import Numeric.Natural

-- | A prism from NonEmpty lists of Natural to NonEmptyHeaps
-- | prop> maybe True (> 0) . previews neh (sum . getHeaps) . fmap aNatural . aNonEmpty
neh :: Prism' (NE.NonEmpty Natural) NonEmptyHeaps
neh = let
  fromNonEmpty = getHeaps
  toNonEmpty = fmap NonEmptyHeaps . mfilter (not . allEmpty) . return
  in prism' fromNonEmpty toNonEmpty

allEmpty :: (F.Foldable t, Num a, Eq a) => t a -> Bool
allEmpty = F.all (== 0)

-- | A wrapper or NonEmpty lists of Natural where at least one value is superior
--   to zero.
newtype NonEmptyHeaps = NonEmptyHeaps { getHeaps :: NE.NonEmpty Natural }
  deriving (Eq, Show, Read)


newtype ANonEmpty a = ANonEmpty { aNonEmpty :: NE.NonEmpty a }
  deriving (Eq, Show, Read)

instance Q.Arbitrary a => Q.Arbitrary (ANonEmpty a) where

  arbitrary = do
    h <- Q.arbitrary
    t <- Q.arbitrary
    return $ ANonEmpty (h NE.:| t)


newtype ANatural = ANatural { aNatural :: Natural }
 deriving (Eq, Show, Read)

instance Q.Arbitrary ANatural where

  arbitrary = do
    x <- Q.arbitrary
    return . ANatural . fromInteger $ Q.getPositive x
