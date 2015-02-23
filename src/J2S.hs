{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module J2S
  ( Inter
  , Action
  , End
  , Err
  , Player
  , BoardInfo
  , nextPlayer
  , executeAction
  , askAction
  , informOnError
  , runGame
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Either

import Control.Monad.Loops

import Data.Either.Combinators (fromLeft')

data family Err b
type family Inter b :: * -> *
type family Action b
type family End b
type family Player b

class BoardInfo b where
  nextPlayer    :: b     -> Player b
  executeAction :: b     -> Action b
                -> ExceptT (Err b) (EitherT (End b) (Inter b)) b
  askAction     :: b     -> Player b -> Inter b (Action b)
  informOnError :: Err b -> Inter b ()

play :: (BoardInfo b, Functor (Inter b), Monad (Inter b))
     => b -> Inter b (End b)
play = fmap fromLeft' -- fromLeft' is ok only Left can get out of iterateM
               . runEitherT . iterateM_ gameEngine

gameEngine :: (BoardInfo b, Functor (Inter b), Monad (Inter b))
           => b -> EitherT (End b) (Inter b) b
gameEngine i = do
  a  <- lift $ askAction <*> nextPlayer $ i
  i' <- runExceptT $ executeAction i a
  either (\e -> lift (informOnError e) >> gameEngine i) return $ i'

runGame :: (BoardInfo b, Functor (Inter b), Monad (Inter b))
        => (forall a. Inter b a -> IO a) -> (End b -> IO ()) -> b -> IO ()
runGame it ds initBoard = it (play initBoard) >>= ds
