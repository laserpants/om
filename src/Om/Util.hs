module Om.Util where

import Data.Text (Text)
import Data.Functor.Foldable

type Name = Text
type Names = [Name]

type Algebra   f a = f a -> a
type Coalgebra f a = a -> f a

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f = ((f <$>) <$>)

infixl 4 <$$>

(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = flip (<$>)

infixl 1 <#>

embed1 :: (Corecursive t) => (t1 -> Base t t) -> t1 -> t
embed1 t a = embed (t a)
{-# INLINE embed1 #-}

embed2 :: (Corecursive t) => (t1 -> t2 -> Base t t) -> t1 -> t2 -> t
embed2 t a b = embed (t a b)
{-# INLINE embed2 #-}

embed3 :: (Corecursive t) => (t1 -> t2 -> t3 -> Base t t) -> t1 -> t2 -> t3 -> t
embed3 t a b c = embed (t a b c)
{-# INLINE embed3 #-}

embed4 :: (Corecursive t) => (t1 -> t2 -> t3 -> t4 -> Base t t) -> t1 -> t2 -> t3 -> t4 -> t
embed4 t a b c d = embed (t a b c d)
{-# INLINE embed4 #-}

embed5 :: (Corecursive t) => (t1 -> t2 -> t3 -> t4 -> t5 -> Base t t) -> t1 -> t2 -> t3 -> t4 -> t5 -> t
embed5 t a b c d e = embed (t a b c d e)
{-# INLINE embed5 #-}
