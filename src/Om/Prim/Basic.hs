{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.Basic 
  ( BasicPrim(..)
  , basicPrelude
  ) where

import Om.Eval
import Om.Prim
import Om.Util

data BasicPrim 
    = Int Int 
    | Bool Bool
    deriving (Show, Eq)

instance PrimType BasicPrim Int where
    toPrim            = Int
    fromPrim (Int n)  = Just n
    fromPrim _        = Nothing

instance PrimType BasicPrim Bool where
    toPrim            = Bool
    fromPrim (Bool b) = Just b
    fromPrim _        = Nothing

basicPrelude :: (Monad m) => [(Name, m (Value BasicPrim m))]
basicPrelude =
    [ ("eq"  , primFun2 ((==) :: Int -> Int -> Bool))
    , ("add" , primFun2 ((+) :: Int -> Int -> Int ))
    , ("sub" , primFun2 ((-) :: Int -> Int -> Int ))
    , ("mul" , primFun2 ((*) :: Int -> Int -> Int ))
    , ("div" , primFun2 (div :: Int -> Int -> Int ))
    ]
