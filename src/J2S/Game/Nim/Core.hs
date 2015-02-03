{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module J2S.Game.Nim.Core
  ( nimRules
  , nimConfig
  , Ongoing
  , Finished
  , activePlayer
  , otherPlayer
  , heaps
  , winner
  , loser
  , nbOfHeaps
  , InteractionF (..)
  , Inter
  , Move
  , MoveError (..)
  , Player (..)
  , PlayerType (..)
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

data Ongoing
  = Ongoing
  { _activePlayer :: Player
  , _otherPlayer   :: Player
  , _heaps        :: NonEmptyHeaps
  }

data Finished
  = Finished
  { _winner    :: Player
  , _loser     :: Player
  , _nbOfHeaps :: Natural
  }

type Move = (Natural, Natural)

data InteractionF a
  = AskMove Player Ongoing (Move -> a)
  | DisplayMove Move a
  | RaiseError MoveError a
  deriving (Functor)

type Inter = Free InteractionF

data MoveError
  = InvalidIndex Natural
  | InvalidTokens Natural

type TurnResult = EitherT MoveError (EitherT Finished Inter) Ongoing 

data Player
  = FirstPlayer PlayerType 
  | SecondPlayer PlayerType
  deriving (Eq, Ord)

data PlayerType = Human | Computer
  deriving (Eq, Ord)

makeLenses '' Ongoing
makeLenses '' Finished

nimRules :: J.GameRules Ongoing Move MoveError Inter Player Finished
nimRules = J.GameRules nextPlayer executeMove askAction informOnError


nimConfig :: PlayerType -> PlayerType -> NonEmptyHeaps -> Ongoing
nimConfig p1 p2 = Ongoing (FirstPlayer p1) (SecondPlayer p2)


nextPlayer :: Ongoing -> Player
nextPlayer = view activePlayer


executeMove :: Move -> Ongoing -> TurnResult
executeMove m o = do
  idx <- modifyHeap m $ buildHeapZipper o
  lift $ afterMove m o idx

afterMove :: Move
          -> Ongoing
          -> NE.NonEmpty Natural
          -> EitherT Finished Inter Ongoing 
afterMove m o h = lift (inform m) >> hoistEither (rebuildInfo o h)

buildHeapZipper :: Ongoing -> Top :>> NE.NonEmpty Natural :>> Natural
buildHeapZipper = fromWithin traverse . zipper . views heaps (review neh)

modifyHeap :: (Monad m)
           => Move
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> EitherT MoveError m (NE.NonEmpty Natural)
modifyHeap (i, n) = moveToIndex i >=> popHeap n 

moveToIndex :: (Monad m)
           => Natural
           -> Top :>> NE.NonEmpty Natural :>> Natural
           -> EitherT MoveError m (Top :>> NE.NonEmpty Natural :>> Natural)
moveToIndex i = let
  handleError = maybe (left $ InvalidIndex i) return
  move = jerks rightward (fromIntegral i) 
  in handleError . move

popHeap :: (Monad m)
        => Natural
        -> Top :>> NE.NonEmpty Natural :>> Natural
        -> EitherT MoveError m (NE.NonEmpty Natural)
popHeap n = let
  handleError = maybe (left $ InvalidTokens n) (return . rezip)
  pop = focus (safeSubtract n)
  in handleError . pop

rebuildInfo :: Ongoing -> NE.NonEmpty Natural -> Either Finished Ongoing
rebuildInfo o h = let
  a = view activePlayer o
  n = view otherPlayer o
  in maybe
      (Left $ Finished a n $ fromIntegral $ NE.length h)
      (Right . Ongoing n a)
      $ preview neh h

inform :: Move -> Inter ()
inform = liftF . flip DisplayMove ()

safeSubtract :: (Ord a, Num a) => a -> a -> Maybe a
safeSubtract x y = guard (x <= y) >> return (y - x)

askAction :: Player -> Ongoing -> Inter Move
askAction p = liftF . flip (AskMove p) id


informOnError :: MoveError -> Inter ()
informOnError = liftF . flip RaiseError ()
