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

import J2S (runGame)
import qualified J2S.Game.Nim.Core as Nim

textNim :: Nim.Ongoing -> IO ()
textNim = runGame Nim.nimRules goInter showScore

goInter :: Nim.Inter a -> IO a
goInter (Pure x) = return x
goInter (Free f) = case f of
  Nim.AskMove p info g -> do
    print $ playerName p <> "'s turn:"
    print $ showInfo info
    i <- askIndex p info
    n <- askNbTokens p i info
    goInter $ g (i, n)
  Nim.DisplayMove (i, n) x -> do
    print $ "Remove " <> T.pack (show n) <> " tokens from heap " <> T.pack (show i)
    goInter x
  Nim.RaiseError e x -> do
    showError e
    goInter x

showInfo :: Nim.Ongoing -> T.Text
showInfo = views Nim.heaps (showHeaps . review Nim.neh)

showHeaps :: NE.NonEmpty Natural -> T.Text
showHeaps h = let
  m = fromIntegral $ F.maximum h
  line = liftA2 mappend (flip replicate 'x') (flip replicate ' ' . (m -))
         . fromIntegral
  toVertical = fmap T.reverse . T.transpose . fmap T.pack . NE.toList
  in T.intercalate "\n" . toVertical $ fmap line h

askIndex :: Nim.Player -> Nim.Ongoing -> IO Natural
askIndex = const . const $ putStrLn "Enter a valid Heap index" >> safeRead

askNbTokens :: Nim.Player -> Natural -> Nim.Ongoing -> IO Natural
askNbTokens =
  const . const . const $ putStrLn "Enter a valid number of tokens" >> safeRead

safeRead :: IO Natural
safeRead = fmap read $ getLine

showError :: Nim.MoveError -> IO ()
showError (Nim.InvalidIndex _)  = putStrLn "This index is invalid"
showError (Nim.InvalidTokens _) = putStrLn "Not enough tokens here"

playerName :: Nim.Player -> T.Text
playerName (Nim.FirstPlayer  _) = "Player1"
playerName (Nim.SecondPlayer _) = "Player2"

showScore :: Nim.Finished -> IO ()
showScore = views Nim.winner (print . (<> " won") . playerName)
