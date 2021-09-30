{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.BasicNats where

import Om.Eval
import Om.Prim
import Om.Util

data BasicNatsPrim 
    = PInt Int 
    | PNat Int
    | PBool Bool
    deriving (Show, Eq)

instance PrimType BasicNatsPrim Int where
    toPrim n           = PInt n
    fromPrim (PInt n)  = Just n
    fromPrim _         = Nothing

instance PrimType BasicNatsPrim Bool where
    toPrim b           = PBool b
    fromPrim (PBool b) = Just b
    fromPrim _         = Nothing

--basicPrelude :: (Monad m) => [(Name, m (Value BasicNatsPrim m))]
--basicPrelude =
--    [ ("eq"  , primFun2 ((\a b -> a == b) :: Int -> Int -> Bool))
--    , ("add" , primFun2 ((\a b -> a + b) :: Int -> Int -> Int ))
--    , ("sub" , primFun2 ((\a b -> a - b) :: Int -> Int -> Int ))
--    , ("mul" , primFun2 ((\a b -> a * b) :: Int -> Int -> Int ))
--    , ("div" , primFun2 ((\a b -> a `div` b) :: Int -> Int -> Int ))
--    ]
