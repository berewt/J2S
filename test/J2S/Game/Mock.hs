{-# LANGUAGE TypeFamilies     #-}

module J2S.Game.Mock where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Trans.Either

import qualified Data.List.NonEmpty         as NE

import           Numeric.Natural

import           Test.QuickCheck

import qualified J2S                        as J



data MockGame
  = MockGame
  { players :: [J.Player MockGame]
  , board :: Integer
  , boardActions :: NE.NonEmpty (J.Action MockGame, Either EndMockGame MockGame) }
  deriving (Eq, Show, Read)

data EndMockGame = EndMockGame { finalBoard :: Integer }
  deriving (Eq, Show, Read)

data instance J.Err MockGame    = Err ()
type instance J.Inter MockGame  = Identity
type instance J.Action MockGame = Int
type instance J.End MockGame    = EndMockGame
type instance J.Player MockGame = Natural


instance J.Game MockGame where

  nextPlayer = head . players

  executeAction b a = maybe (throwError $ Err ()) return
                      . lookup a . NE.toList $ boardActions b

  informAction = const $ const $ hoistEither

  askAction = const . const $ return 42

  informOnError = const $ return ()

instance J.ListableActions MockGame where

  actions = fmap fst . boardActions

instance Arbitrary MockGame where

  arbitrary = do
    n <- fromIntegral . getPositive <$> (arbitrary :: Gen (Positive Int))
    gameForN n

instance Arbitrary EndMockGame where

  arbitrary = EndMockGame <$> arbitrary

gameForN :: Natural -> Gen MockGame
gameForN n = MockGame
             <$> (replicate <$> arbitrary <*> elements [0.. pred n])
             <*> arbitrary
             <*> ((NE.:|) <$> arbitrary <*> arbitrary)
