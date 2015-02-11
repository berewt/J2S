{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module J2S.Game.Nim.Core
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
  , J.Err (..)
  , neh
  ) where

import Control.Lens
import Control.Zipper

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Either

import qualified Data.List.NonEmpty as NE

import Numeric.Natural

import qualified J2S as J
import J2S.Game.Nim.Types

data Nim
  = Nim
  { _activePlayer :: J.Player Nim
  , _otherPlayer   :: J.Player Nim
  , _heaps        :: NonEmptyHeaps
  }

data EndNim
  = EndNim
  { _winner    :: J.Player Nim
  , _loser     :: J.Player Nim
  , _nbOfHeaps :: Natural
  }

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
  deriving (Eq, Ord)

type instance J.Player Nim = NimPlayer

data PlayerType = Human | Computer
  deriving (Eq, Ord)

makeLenses '' Nim
makeLenses '' EndNim

nimConfig :: PlayerType -> PlayerType -> NonEmptyHeaps -> Nim
nimConfig p1 p2 = Nim (FirstPlayer p1) (SecondPlayer p2)

instance J.BoardInfo Nim where

  nextPlayer = view activePlayer

  executeAction o m = do
    idx <- modifyHeap m $ buildHeapZipper o
    lift $ afterAction o m idx

  askAction i p = liftF $ AskAction p i id

  informOnError = liftF . flip RaiseError ()

afterAction :: Nim
          -> J.Action Nim
          -> NE.NonEmpty Natural
          -> EitherT (J.End Nim) (J.Inter Nim) Nim
afterAction o m h = lift (inform m) >> hoistEither (rebuildInfo o h)

buildHeapZipper :: Nim -> Top :>> NE.NonEmpty Natural :>> Natural
buildHeapZipper = fromWithin traverse . zipper . views heaps (review neh)

modifyHeap :: (Monad m)
           => J.Action Nim
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> EitherT (J.Err Nim) m (NE.NonEmpty Natural)
modifyHeap (i, n) = moveToIndex i >=> popHeap n

moveToIndex :: (Monad m)
           => Natural
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> EitherT (J.Err Nim) m (Top :>> NE.NonEmpty Natural :>> Natural)
moveToIndex i = let
  handleError = maybe (left $ InvalidIndex i) return
  move = jerks rightward (fromIntegral i)
  in handleError . move

popHeap :: (Monad m)
        => Natural
        -> Top :>> NE.NonEmpty Natural :>> Natural
        -> EitherT (J.Err Nim) m (NE.NonEmpty Natural)
popHeap n = let
  handleError = maybe (left $ InvalidTokens n) (return . rezip)
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

inform :: J.Action Nim -> J.Inter Nim ()
inform = liftF . flip DisplayAction ()

safeSubtract :: (Ord a, Num a) => a -> a -> Maybe a
safeSubtract x y = guard (x <= y) >> return (y - x)
