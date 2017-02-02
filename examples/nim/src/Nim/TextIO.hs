{-# LANGUAGE OverloadedStrings #-}

module Nim.TextIO
  ( textNim
  ) where

import qualified Data.Foldable          as F
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import qualified Data.Text.IO           as T


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Control.Monad.Reader

import           Data.Monoid            (mappend, (<>))

import           Numeric.Natural

import           J2S                    as J
import           Nim.AI
import           Nim.Core

textNim :: RandomGen g => g -> Nim -> IO ()
textNim gen = runGame (flip evalRandT gen . goInter) showScore

goInter :: (MonadRandom m, MonadIO m) => Inter Nim a -> m a
goInter (Pure x) = return x
goInter (Free f) = case f of
  AskAction p info g -> do
    liftIO . T.putStrLn $ playerName p <> "'s turn:"
    liftIO . T.putStrLn $ showInfo info
    a <- askPlayer p info
    goInter $ g a
  DisplayAction (i, n) x -> do
    liftIO . T.putStrLn
      $ "Remove " <> T.pack (show n) <> " tokens from heap " <> T.pack (show i)
    goInter x
  RaiseError e x ->
    liftIO $ showError e >> goInter x

askPlayer :: (MonadRandom m, MonadIO m) => Player Nim -> Nim -> m (Action Nim)
askPlayer p info = case playerType p of
  Human -> do
    i <- askIndex
    n <- askNbTokens
    return (i, n)
  Computer Random -> J.rand info
  Computer MinMax -> return . flip runReader (buildMMParam p info) $ J.minMaxAB info

buildMMParam :: Player Nim -> Nim -> J.MinMaxParam Nim TrivialValuation
buildMMParam p _ = let
  depth = 4
  eval = case p of
              (FirstPlayer  _) -> trivialEvalP1
              (SecondPlayer _) -> trivialEvalP2
  in J.MinMaxParam depth eval

showInfo :: Nim -> T.Text
showInfo = views heaps (showHeaps . review neh)

showHeaps :: NE.NonEmpty Natural -> T.Text
showHeaps h = let
  m = fromIntegral $ F.maximum h
  line = liftA2 mappend (flip replicate 'x') (flip replicate ' ' . (m -))
         . fromIntegral
  toVertical = T.transpose . fmap (T.reverse . T.pack) . NE.toList
  in T.intercalate "\n" . toVertical $ fmap line h

askIndex :: MonadIO m => m Natural
askIndex = liftIO $ putStrLn "Enter a valid Heap index" >> safeRead

askNbTokens :: MonadIO m => m Natural
askNbTokens = liftIO $ putStrLn "Enter a valid number of tokens" >> safeRead

safeRead :: IO Natural
safeRead = read <$> getLine

showError :: Err Nim -> IO ()
showError (InvalidIndex _)  = putStrLn "This index is invalid"
showError (InvalidTokens 0) = putStrLn "You should at least remove one token"
showError (InvalidTokens _) = putStrLn "Not enough tokens here"

playerName :: Player Nim -> T.Text
playerName (FirstPlayer  _) = "Player1"
playerName (SecondPlayer _) = "Player2"

showScore :: End Nim -> IO ()
showScore = views winner (T.putStrLn . (<> " won") . playerName)
