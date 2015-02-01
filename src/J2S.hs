{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module J2S
  ( GameRules (..)
  , runGame
  ) where

import ClassyPrelude

import Control.Lens

import Control.Monad.Loops
import Control.Monad.Trans.Either

import Data.Either.Combinators (fromLeft)
import qualified Data.Map as M (Map, empty)

data GameRules info action err inter player score
  = GameRules
  { _nextPlayer    :: info   -> (Either (M.Map player score)) player
  , _executeAction :: action -> info -> EitherT err inter info
  , _askAction     :: player -> info -> inter action
  , _informOnError :: err    -> inter ()
  }

makeLenses ''GameRules

play :: (Functor inter, Monad inter)
     => GameRules info action err inter player score
     -> info
     -> inter (M.Map player score)
play rules = fmap (fromLeft M.empty) . runEitherT . iterateM_ (gameEngine rules)

gameEngine :: (Functor inter, Monad inter)
     => GameRules info action err inter player score
     -> info
     -> EitherT (M.Map player score) inter info
gameEngine rules i = do
  p <- hoistEither $ view nextPlayer rules i
  lift $ playTurn rules i p

playTurn :: (Monad inter)
     => GameRules info action err inter player score
     -> info
     -> player
     -> inter info
playTurn bg i p = do
  a  <- view askAction bg p i
  i' <- runEitherT $ view executeAction bg a i
  either (\e -> view informOnError bg e >> playTurn bg i p) return $ i'

runGame :: (Functor inter, Monad inter)
        => GameRules info action err inter player score
        -> (forall a. inter a -> IO a)
        -> (Map player score -> IO ())
        -> info
        -> IO ()
runGame rules it ds initBoard = it (play rules initBoard) >>= ds 
