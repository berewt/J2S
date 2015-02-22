{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Data.NLTree
  ( NLTree (..)
  , Prim (..)
  , toTree
  ) where

import Control.Applicative
import Control.Monad

import Data.Functor.Foldable
import qualified Data.Tree as T
import qualified Data.List.NonEmpty as NE

data NLTree n l
  = Node n (NE.NonEmpty (NLTree n l))
  | Leaf l
  deriving (Eq, Read, Show)


data instance Prim (NLTree n l) x
  = N n (NE.NonEmpty x)
  | L l
  deriving (Functor, Read, Show)

type instance Base (NLTree n l) = Prim (NLTree n l)

instance Foldable (NLTree n l) where

  project (Node n xs) = N n xs
  project (Leaf l)    = L l

  para f (Node n xs) = f $ N n (fmap ((,) <*> para f) xs)
  para f (Leaf l)    = f $ L l

instance Unfoldable (NLTree n l) where

  embed (N n xs) = Node n xs
  embed (L l)    = Leaf l

  apo f a = case f a of
                 N n xs -> Node n $ fmap (either id $ apo f) xs
                 L l    -> Leaf l

toTree :: NLTree n l -> T.Tree (Either n l)
toTree (Node n xs) = T.Node (Left n) (fmap toTree $ NE.toList xs)
toTree (Leaf l)    = T.Node (Right l) mzero
