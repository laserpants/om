{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.Basic where

import Om.Eval
import Om.Prim
import Om.Util

data BasicPrim 
    = Int Int 
    | Bool Bool
    deriving (Show, Eq)

instance PrimType BasicPrim Int where
    toPrim n          = Int n
    fromPrim (Int n)  = Just n
    fromPrim _        = Nothing

instance PrimType BasicPrim Bool where
    toPrim b          = Bool b
    fromPrim (Bool b) = Just b
    fromPrim _        = Nothing

basicPrelude :: (Monad m) => [(Name, m (Value BasicPrim m))]
basicPrelude =
    [ ("eq"  , primFun2 ((\a b -> a == b) :: Int -> Int -> Bool))
    , ("add" , primFun2 ((\a b -> a + b) :: Int -> Int -> Int ))
    , ("sub" , primFun2 ((\a b -> a - b) :: Int -> Int -> Int ))
    , ("mul" , primFun2 ((\a b -> a * b) :: Int -> Int -> Int ))
    , ("div" , primFun2 ((\a b -> a `div` b) :: Int -> Int -> Int ))
    ]
