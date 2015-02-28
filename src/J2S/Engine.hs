{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module J2S.Engine
  ( Inter
  , Action
  , End
  , Err
  , Player
  , BoardInfo (..)
  , runGame
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except

import Control.Monad.Loops

import Data.Either.Combinators (fromLeft')

data family Err b
type family Inter b :: * -> *
type family Action b
type family End b
type family Player b

class BoardInfo b where
  nextPlayer    :: b     -> Player b
  executeAction :: b     -> Action b -> Except (Err b) (Either (End b) b)
  informAction  :: b     -> Action b -> Either (End b) b
                -> EitherT (End b) (Inter b) b
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
  i' <- runExceptT $ runAction i a
  either (\e -> lift (informOnError e) >> gameEngine i) return i'

runAction :: (BoardInfo b, Monad (Inter b))
          => b
          -> Action b
          -> ExceptT (Err b) (EitherT (End b) (Inter b)) b
runAction b ac =
  either throwE (lift . informAction b ac) . runExcept $ executeAction b ac

runGame :: (BoardInfo b, Functor (Inter b), Monad (Inter b), Monad m)
        => (forall a. Inter b a -> m a) -> (End b -> m ()) -> b -> m ()
runGame it ds initBoard = it (play initBoard) >>= ds
