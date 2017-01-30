{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.NLTree
  ( NLTree (..)
  , NLTreeF (..)
  , toTree
  ) where

import           Control.Monad

import           Data.Functor.Foldable
import qualified Data.Tree             as T
import qualified Data.List.NonEmpty    as NE

import qualified Test.QuickCheck       as Q


data NLTree n l
  = Node n (NE.NonEmpty (NLTree n l))
  | Leaf l
  deriving (Eq, Read, Show)


data NLTreeF n l x
  = N n (NE.NonEmpty x)
  | L l
  deriving (Functor, Read, Show)

type instance Base (NLTree n l) = NLTreeF n l

instance Recursive (NLTree n l) where

  project (Node n xs) = N n xs
  project (Leaf l)    = L l

  para f (Node n xs) = f $ N n (fmap ((,) <*> para f) xs)
  para f (Leaf l)    = f $ L l

instance Corecursive (NLTree n l) where

  embed (N n xs) = Node n xs
  embed (L l)    = Leaf l

  apo f a = case f a of
                 N n xs -> Node n $ fmap (either id $ apo f) xs
                 L l    -> Leaf l

instance (Q.Arbitrary n, Q.Arbitrary l) => Q.Arbitrary (NLTree n l) where

  arbitrary = Q.oneof [aNode, aLeaf]

aLeaf :: Q.Arbitrary l => Q.Gen (NLTree n l)
aLeaf = Leaf <$> Q.arbitrary

aNode :: (Q.Arbitrary n, Q.Arbitrary l) => Q.Gen (NLTree n l)
aNode = let
  cs = (NE.:|) <$> Q.arbitrary <*> Q.arbitrary
  in Node <$> Q.arbitrary <*> cs

toTree :: NLTree n l -> T.Tree (Either n l)
toTree (Node n xs) = T.Node (Left n) (toTree <$> NE.toList xs)
toTree (Leaf l)    = T.Node (Right l) mzero
