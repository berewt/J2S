{-# LANGUAGE OverloadedStrings #-}

module J2S.Game.Nim.TextIO
  ( textNim
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Free

import Numeric.Natural

import Data.Monoid ((<>), mappend)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import J2S
import J2S.Game.Nim.Core

textNim :: Nim -> IO ()
textNim = runGame goInter showScore

goInter :: Inter Nim a -> IO a
goInter (Pure x) = return x
goInter (Free f) = case f of
  AskAction p info g -> do
    T.putStrLn $ playerName p <> "'s turn:"
    T.putStrLn $ showInfo info
    i <- askIndex p info
    n <- askNbTokens p i info
    goInter $ g (i, n)
  DisplayAction (i, n) x -> do
    T.putStrLn $ "Remove " <> T.pack (show n) <> " tokens from heap " <> T.pack (show i)
    goInter x
  RaiseError e x -> do
    showError e
    goInter x

showInfo :: Nim -> T.Text
showInfo = views heaps (showHeaps . review neh)

showHeaps :: NE.NonEmpty Natural -> T.Text
showHeaps h = let
  m = fromIntegral $ F.maximum h
  line = liftA2 mappend (flip replicate 'x') (flip replicate ' ' . (m -))
         . fromIntegral
  toVertical = T.transpose . fmap (T.reverse . T.pack) . NE.toList
  in T.intercalate "\n" . toVertical $ fmap line h

askIndex :: Player Nim -> Nim -> IO Natural
askIndex = const . const $ putStrLn "Enter a valid Heap index" >> safeRead

askNbTokens :: Player Nim -> Natural -> Nim -> IO Natural
askNbTokens =
  const . const . const $ putStrLn "Enter a valid number of tokens" >> safeRead

safeRead :: IO Natural
safeRead = fmap read $ getLine

showError :: Err Nim -> IO ()
showError (InvalidIndex _)  = putStrLn "This index is invalid"
showError (InvalidTokens _) = putStrLn "Not enough tokens here"

playerName :: Player Nim -> T.Text
playerName (FirstPlayer  _) = "Player1"
playerName (SecondPlayer _) = "Player2"

showScore :: End Nim -> IO ()
showScore = views winner (T.putStrLn . (<> " won") . playerName)
