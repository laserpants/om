{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.BasicNats where

import Om.Eval
import Om.Prim
import Om.Util

data BasicNatsPrim 
    = Int Int 
    | Nat Integer
    | Bool Bool
    deriving (Show, Eq)

instance PrimType BasicNatsPrim Int where
    toPrim n          = Int n
    fromPrim (Int n)  = Just n
    fromPrim _        = Nothing

instance PrimType BasicNatsPrim Bool where
    toPrim b          = Bool b
    fromPrim (Bool b) = Just b
    fromPrim _        = Nothing

instance PrimType BasicNatsPrim Integer where
    toPrim n          = Nat n
    fromPrim (Nat n)  = Just n
    fromPrim _        = Nothing

basicNatsPrelude :: (Monad m) => [(Name, m (Value BasicNatsPrim m))]
basicNatsPrelude =
    [ ("add" , primFun2 ((\a b -> a + b) :: Integer -> Integer -> Integer ))
    ]
