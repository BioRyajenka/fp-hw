{-# LANGUAGE RankNTypes #-}

module Task2
       (
         set
       , view
       , over
       , _1
       , _2
       , lens
       ) where

import Data.Functor.Identity
import Data.Functor.Const

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-- forall f . Functor f => (a -> f a) -> s -> f s
set :: Lens' s a -> a -> s -> s
set lenz value state = runIdentity $ lenz (\_ -> Identity value) state

view :: Lens' s a -> s -> a
view lenz state = getConst $ lenz Const state

over :: Lens' s a -> (a -> a) -> s -> s
over lenz f state = runIdentity $ lenz (Identity . f) state


--_1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 k p = fmap (\a -> (a, snd p)) (k (fst p))

--_2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 k p = fmap (\a -> (fst p, a)) (k (snd p))

--lens :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter k p = fmap (setter p) (k (getter p))

-- Объединить две линзы в одну, которая работает с Either.
{-choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
--choosing l1 l2 = lens _3 _4
choosing l1 l2 = lens (either (view l1) (view l2)) _4
-}
