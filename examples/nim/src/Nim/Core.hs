{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Nim.Core
  ( nimConfig
  , Nim
  , activePlayer
  , otherPlayer
  , heaps
  , winner
  , loser
  , nbOfHeaps
  , InteractionF (..)
  , PlayerType (..)
  , NimPlayer (..)
  , playerType
  , J.Err (..)
  , Strategy (..)
  , neh
  ) where

import           Control.Lens
import           Control.Zipper

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Trans.Either

import qualified Data.List.NonEmpty         as NE

import           Numeric.Natural

import qualified Test.QuickCheck            as Q

import qualified J2S                        as J
import           Nim.Types

data Nim
  = Nim
  { _activePlayer :: J.Player Nim
  , _otherPlayer  :: J.Player Nim
  , _heaps        :: NonEmptyHeaps
  } deriving (Eq, Read, Show)

data EndNim
  = EndNim
  { _winner    :: J.Player Nim
  , _loser     :: J.Player Nim
  , _nbOfHeaps :: Natural
  } deriving (Eq, Read, Show)

type instance J.End Nim = EndNim

type instance J.Action Nim = (Natural, Natural)

data InteractionF a
  = AskAction (J.Player Nim) Nim (J.Action Nim -> a)
  | DisplayAction (J.Action Nim) a
  | RaiseError (J.Err Nim) a
  deriving (Functor)

type instance J.Inter Nim = Free InteractionF

data instance J.Err Nim
  = InvalidIndex Natural
  | InvalidTokens Natural

data NimPlayer
  = FirstPlayer PlayerType
  | SecondPlayer PlayerType
  deriving (Eq, Ord, Read, Show)

type instance J.Player Nim = NimPlayer

playerType :: NimPlayer -> PlayerType
playerType (FirstPlayer  p) = p
playerType (SecondPlayer p) = p

data Strategy = Random | MinMax
  deriving (Eq, Ord, Read, Show)

data PlayerType = Human | Computer Strategy
  deriving (Eq, Ord, Read, Show)

makeLenses '' Nim
makeLenses '' EndNim

-- | Create a Nim game
nimConfig :: PlayerType    -- | First Player's Type
          -> PlayerType    -- | Second Player's Type
          -> NonEmptyHeaps -- | Initial Heaps Configuration
          -> Nim
nimConfig p1 p2 = Nim (FirstPlayer p1) (SecondPlayer p2)


instance J.Game Nim where

  nextPlayer = view activePlayer

  executeAction b m = rebuildInfo b <$> modifyHeap m (buildHeapZipper b)

  informAction _ a r =
    lift (liftF $ DisplayAction  a ()) >> hoistEither r

  askAction i p = liftF $ AskAction p i id

  informOnError = liftF . flip RaiseError ()


instance J.ListableActions Nim where

 actions = let
   go i n = (,) (fromIntegral i) <$> if n == 0 then [] else enumFromThenTo n (n - 1) 1
   in views (heaps . re neh) (NE.fromList . ifoldMap go)


-- Helpers for actions

buildHeapZipper :: Nim -> Top :>> NE.NonEmpty Natural :>> Natural
buildHeapZipper = views (heaps . re neh) (fromWithin traverse . zipper)

modifyHeap :: Monad m
           => J.Action Nim
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> ExceptT (J.Err Nim) m (NE.NonEmpty Natural)
modifyHeap (i, n) = moveToIndex i >=> popHeap n

moveToIndex :: Monad m
           => Natural
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> ExceptT (J.Err Nim) m (Top :>> NE.NonEmpty Natural :>> Natural)
moveToIndex i = let
  handleError = maybe (throwError $ InvalidIndex i) return
  move = jerks rightward (fromIntegral i)
  in handleError . move

popHeap :: (Monad m)
        => Natural
        -> Top :>> NE.NonEmpty Natural :>> Natural
        -> ExceptT (J.Err Nim) m (NE.NonEmpty Natural)
popHeap n = let
  handleError = maybe (throwError $ InvalidTokens n) (return . rezip)
  pop = focus (safeSubtract n)
  in handleError . pop

rebuildInfo :: Nim -> NE.NonEmpty Natural -> Either (J.End Nim) Nim
rebuildInfo o h = let
  a = view activePlayer o
  n = view otherPlayer o
  in maybe
      (Left $ EndNim a n $ fromIntegral $ NE.length h)
      (Right . Nim n a)
      $ preview neh h


-- Helper for Naturals

safeSubtract :: (Ord a, Num a) => a -> a -> Maybe a
safeSubtract x y = guard (x > 0 && x <= y) >> return (y - x)


-- QuickCheck instances

instance Q.Arbitrary Nim where

  arbitrary = uncurry Nim <$> arbitraryPlayers <*> Q.arbitrary

instance Q.Arbitrary EndNim where

  arbitrary = uncurry EndNim <$> arbitraryPlayers <*> (aNatural <$> Q.arbitrary)

arbitraryPlayers :: Q.Gen (NimPlayer, NimPlayer)
arbitraryPlayers = let
  p1 = FirstPlayer  <$> Q.arbitrary
  p2 = SecondPlayer <$> Q.arbitrary
  in Q.oneof [(,) <$> p1 <*> p2, (,) <$> p2 <*> p1]

instance Q.Arbitrary Strategy where
  arbitrary = Q.elements [Random, MinMax]

instance Q.Arbitrary PlayerType where
  arbitrary = Q.oneof [return Human, Computer <$> Q.arbitrary]
