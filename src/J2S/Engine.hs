{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

 {-|

Module: J2S.Engine
Description: J2S module for game description
Copyright: © Nicolas Biri, 2015-2017
License: MIT
Maintainer: nicolas@biri.name
Portability: POSIX

  This modules contain the generic definition for the definition of a game.
  A game is defined by the definition of a datatype that gather all the game information
  And an typeclass instance of 'BoardInfo' for ths datatype. The 'BoardInfo' typeclass
  defines the functions required to implements the game logic.

-}
module J2S.Engine
  ( Inter
  , Action
  , End
  , Err
  , Player
  , BoardInfo (..)
  , runGame
  ) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except

import           Control.Monad.Loops

import           Data.Either.Combinators    (fromLeft')

-- | Define the error for a Game `b`.
-- | An error traditionnaly occurs when a player interact with the board in an unexpected way
data family Err b

-- | This type is used to define all the informations
-- | that are raised to the player (either for display, as a human or for strategic purpose for an AI)
type family Inter b :: * -> *

-- | The interaction with the game `b`.
type family Action b

-- | The type that define the different end games
type family End b

-- | The different kind of players available for the game.
type family Player b

-- | This class define the wohe behaviour of a game
class BoardInfo b where
  -- | Given a ongoing game configuration, returns the next player
  nextPlayer    :: b     -> Player b
  -- | Apply an action to a game configuration. Returns an error if the Actin is not allowed.
  -- Otherwise, it returns either a end game result or a new game configuration
  executeAction :: b     -> Action b -> Except (Err b) (Either (End b) b)
  -- | Given the initial board, a valid action and its result, it returns an interaction
  -- to display if any.
  informAction  :: b     -> Action b -> Either (End b) b
                -> EitherT (End b) (Inter b) b
  -- | Given a game context and a player, returns the interation required to ask this player
  -- her next move
  askAction     :: b     -> Player b -> Inter b (Action b)
  -- | Generate the interaction to inform everyone of an error
  informOnError :: Err b -> Inter b ()

-- | Play a game from its initial configuration to the end
play :: (BoardInfo b, Monad (Inter b))
     => b -> Inter b (End b)
play = fmap fromLeft' -- fromLeft' is ok: only Left can get out of iterateM
               . runEitherT . iterateM_ gameEngine

-- | Play a game turn
-- (repeatidly ask an action to the active player until she provides a valid one)
gameEngine :: (BoardInfo b, Monad (Inter b))
           => b -> EitherT (End b) (Inter b) b
gameEngine i = do
  a  <- lift $ askAction <*> nextPlayer $ i
  i' <- runExceptT $ runAction i a
  either (\e -> lift (informOnError e) >> gameEngine i) return i'

-- | Run on the game the given action
runAction :: (BoardInfo b, Monad (Inter b))
          => b
          -> Action b
          -> ExceptT (Err b) (EitherT (End b) (Inter b)) b
runAction b ac =
  either throwE (lift . informAction b ac) . runExcept $ executeAction b ac

-- | execute the game with the given interpreter for interactions
runGame :: (BoardInfo b, Monad (Inter b), Monad m)
        => (forall a. Inter b a -> m a) -> (End b -> m ()) -> b -> m ()
runGame it ds initBoard = it (play initBoard) >>= ds
