module Task5
       ( maybeConcat
       , eitherConcat
       , NonEmpty
       , Identity (..)
       ) where

import Data.Maybe(fromJust)
import Data.Either(lefts, rights)
import Data.Semigroup(Semigroup, (<>))

import Task3(Tree (..), tInsert)

{- 1 -}

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat xs = fromJust $ mconcat (Just []:xs)
--maybeConcat xs = fromJust $ foldl mappend (Just []) xs

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = (mconcat $ lefts xs, mconcat $ rights xs)

{- 2 -}

data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where
    (a :| as) <> (b :| bs) = a :| (as ++ b : bs)
 
{-sconcat :: Semigroup a => NonEmpty a -> a
sconcat (a :| as) = f a as where
    f b (c:cs) = b <> f c cs
    f b _      = b-}

newtype Identity a = Identity { runIdentity :: a } deriving (Show)
instance (Monoid a) => Monoid (Identity a) where
    mempty                            = Identity mempty
    mappend (Identity a) (Identity b) = Identity (mappend a b)

{- 3 -}
instance (Ord a) => Monoid (Tree a) where
    mempty = Leaf

    mappend t Leaf         = t
    mappend t (Node v l r) = mappend (tInsert (mappend t l) v) r

instance (Ord a) => Semigroup (Tree a) where
    (<>) = mappend