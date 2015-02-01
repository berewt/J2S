{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module J2S.Game.Nim.Core
  ( nimRules
  , nimConfig
  , Info
  , Finished
  , Ongoing
  , activePlayer
  , otherPlayer
  , heaps
  , winner
  , loser
  , nbOfHeaps
  , InteractionF (..)
  , Move
  , Player (..)
  , PlayerType (..)
  , Score
  ) where

import ClassyPrelude

import Control.Lens
import Control.Zipper

import Control.Monad.Free
import Control.Monad.Trans.Either

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Numeric.Natural

import qualified J2S as J
import J2S.Game.Nim.Types

type Info = Either Finished Ongoing 

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
  = AskMove Player Info (Move -> a)
  | DisplayMove Move a
  | RaiseError MoveError a
  deriving (Functor)

type Inter = Free InteractionF

data MoveError
  = InvalidIndex Natural
  | InvalidTokens Natural
  | NoMoveAvailable

data Player
  = FirstPlayer PlayerType 
  | SecondPlayer PlayerType
  deriving (Eq, Ord)

data PlayerType = Human | Computer
  deriving (Eq, Ord)

data Score = Win | Lose

makeLenses '' Ongoing
makeLenses '' Finished

nimRules :: J.GameRules Info Move MoveError Inter Player Score
nimRules = J.GameRules nextPlayer executeAction askAction informOnError

nimConfig :: PlayerType -> PlayerType -> NonEmptyHeaps -> Info
nimConfig p1 p2 = return . Ongoing (FirstPlayer p1) (SecondPlayer p2)

nextPlayer :: Info -> Either (Map Player Score) Player
nextPlayer = bimap
  ( M.fromList . sequence  [ views winner $ flip (,) Win
                           , views loser  $ flip (,) Lose 
                           ]
  )
  ( view activePlayer )


executeAction :: Move -> Info -> EitherT MoveError Inter Info
executeAction m = either (const $ left NoMoveAvailable) (executeMove m)  

executeMove :: Move -> Ongoing -> EitherT MoveError Inter Info
executeMove m =
  (=<<) . (afterMove m) <*> modifyHeap m . buildHeapZipper

afterMove :: Move
          -> Ongoing
          -> NE.NonEmpty Natural
          -> EitherT MoveError Inter Info
afterMove m o h = do
  lift $ (inform m)
  rebuildInfo o h

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

rebuildInfo :: Ongoing -> NE.NonEmpty Natural
            -> EitherT MoveError Inter Info
rebuildInfo o h = let
  a = view activePlayer o
  n = view otherPlayer o
  in return $ maybe
      (Left $ Finished a n $ fromIntegral $ length h)
      (Right . Ongoing n a)
       $ preview neh h

inform :: Move -> Inter ()
inform = liftF . flip DisplayMove ()

safeSubtract :: (Ord a, Num a) => a -> a -> Maybe a
safeSubtract x y = guard (x <= y) >> return (y - x)


askAction :: Player -> Info -> Inter Move
askAction p = liftF . flip (AskMove p) id


informOnError :: MoveError -> Inter ()
informOnError = liftF . flip RaiseError ()
