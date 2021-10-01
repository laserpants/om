{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim 
  ( PrimType(..)
  , Fun
  , fun1
  , fun2
  , fun3
  , fun4
  , fun5
  , arity
  , applyFun 
  ) where

class PrimType p t where
    toPrim   :: t -> p
    fromPrim :: p -> Maybe t

data Fun p
    = Fun1 (p -> Maybe p)
    | Fun2 (p -> p -> Maybe p)
    | Fun3 (p -> p -> p -> Maybe p)
    | Fun4 (p -> p -> p -> p -> Maybe p)
    | Fun5 (p -> p -> p -> p -> p -> Maybe p)

fun1 :: (PrimType p a, PrimType p b) => (a -> b) -> Fun p
fun1 f = Fun1 (\a -> let b = f <$> fromPrim a in toPrim <$> b)

fun2 :: (PrimType p a, PrimType p b, PrimType p c) => (a -> b -> c) -> Fun p
fun2 f = Fun2 (\a b -> let c = f <$> fromPrim a <*> fromPrim b in toPrim <$> c)

fun3 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d) => (a -> b -> c -> d) -> Fun p
fun3 f = Fun3 (\a b c -> let d = f <$> fromPrim a <*> fromPrim b <*> fromPrim c in toPrim <$> d)

fun4 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e) => (a -> b -> c -> d -> e) -> Fun p
fun4 f = Fun4 (\a b c d -> let e = f <$> fromPrim a <*> fromPrim b <*> fromPrim c <*> fromPrim d in toPrim <$> e)

fun5 :: (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e, PrimType p f) => (a -> b -> c -> d -> e -> f) -> Fun p
fun5 f = Fun5 (\a b c d e -> let g = f <$> fromPrim a <*> fromPrim b <*> fromPrim c <*> fromPrim d <*> fromPrim e in toPrim <$> g)

arity :: Fun p -> Int
arity = \case
    Fun1 _ -> 1
    Fun2 _ -> 2
    Fun3 _ -> 3
    Fun4 _ -> 4
    Fun5 _ -> 5

applyFun :: Fun p -> [p] -> Maybe p
applyFun fun args =
    case fun of
        Fun1 f -> f (head args)
        Fun2 f -> f (head args) (args !! 1)
        Fun3 f -> f (head args) (args !! 1) (args !! 2)
        Fun4 f -> f (head args) (args !! 1) (args !! 2) (args !! 3)
        Fun5 f -> f (head args) (args !! 1) (args !! 2) (args !! 3) (args !! 4)
