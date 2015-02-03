{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module J2S
  ( GameRules (..)
  , runGame
  ) where

import Control.Lens

import Control.Monad.Trans
import Control.Monad.Trans.Either

import Control.Monad.Loops

import Data.Either.Combinators (fromLeft')

data GameRules info action err inter player end
  = GameRules
  { _nextPlayer    :: info   -> player
  , _executeAction :: action -> info -> EitherT err (EitherT end inter) info
  , _askAction     :: player -> info -> inter action
  , _informOnError :: err    -> inter ()
  }

makeLenses ''GameRules

play :: (Functor inter, Monad inter)
     => GameRules info action err inter player end
     -> info
     -> inter end
play rules = fmap fromLeft' -- fromLeft' is ok only Left can get out of iterateM
               . runEitherT . iterateM_ (gameEngine rules)

gameEngine :: (Functor inter, Monad inter)
     => GameRules info action err inter player end
     -> info
     -> EitherT end inter info
gameEngine r i = let
  p = view nextPlayer r i
  in do
    a  <- lift $ view askAction r p i
    i' <- runEitherT $ view executeAction r a i
    either (\e -> lift (view informOnError r e) >> gameEngine r i) return $ i'

runGame :: (Functor inter, Monad inter)
        => GameRules info action err inter player end
        -> (forall a. inter a -> IO a)
        -> (end -> IO ())
        -> info
        -> IO ()
runGame rules it ds initBoard = it (play rules initBoard) >>= ds 
