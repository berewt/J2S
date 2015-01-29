{-# LANGUAGE TemplateHaskell #-}

module J2S
  ( play
  , BoardGame
  ) where

import ClassyPrelude

import Control.Lens

import Control.Monad.Error
import Control.Monad.Loops
import Control.Monad.Trans.Either

import Data.Either.Combinators (fromLeft)
import qualified Data.Map as M (Map, empty)

data BoardGame info action err inter player score
  = BoardGame
  { _nextPlayer    :: info   -> (Either (M.Map player score)) player
  , _executeAction :: action -> info -> ErrorT err inter info
  , _askAction     :: player -> info -> inter action
  , _informOnError :: err    -> inter ()
  }

makeLenses ''BoardGame

play :: (Functor inter, Monad inter)
     => BoardGame info action err inter player score
     -> info
     -> inter (M.Map player score)
play bg = fmap (fromLeft M.empty) . runEitherT . iterateM_ (gameEngine bg)

gameEngine :: (Functor inter, Monad inter)
     => BoardGame info action err inter player score
     -> info
     -> EitherT (M.Map player score) inter info
gameEngine bg i = do
  p <- hoistEither $ view nextPlayer bg i
  lift $ playTurn bg i p

playTurn :: (Monad inter)
     => BoardGame info action err inter player score
     -> info
     -> player
     -> inter info
playTurn bg i p = do
  a  <- view askAction bg p i
  i' <- runErrorT $ view executeAction bg a i
  either (\e -> view informOnError bg e >> playTurn bg i p) return $ i'
