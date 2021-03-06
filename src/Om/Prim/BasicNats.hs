{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.BasicNats
  ( BasicNatsPrim(..)
  , basicNatsPrelude
  ) where

import Om.Eval
import Om.Prim
import Om.Util
import Om.Plug.Nats

data BasicNatsPrim
    = Int Int
    | Nat Integer
    | Bool Bool
    deriving (Show, Eq)

instance PrimType BasicNatsPrim Int where
    toPrim                 = Int
    fromPrim (Int n)       = Just n
    fromPrim _             = Nothing

instance PrimType BasicNatsPrim Bool where
    toPrim                 = Bool
    fromPrim (Bool b)      = Just b
    fromPrim _             = Nothing

instance PrimType BasicNatsPrim NatType where
    toPrim (FromInteger n) = Nat n
    fromPrim (Nat n)       = Just (FromInteger n)
    fromPrim _             = Nothing

basicNatsPrelude :: (Monad m) => [(Name, m (Value BasicNatsPrim m))]
basicNatsPrelude =
    [ ("add"    , primFun2 ((+) :: NatType -> NatType -> NatType))
    , ("pack"   , primFun1 pack)
    , ("unpack" , primFun1 unpack)
    ]

pack :: Int -> NatType
pack = FromInteger . fromIntegral

unpack :: NatType -> Int
unpack (FromInteger n) = fromIntegral n
