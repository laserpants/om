{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
module Om.Prim.Basic where

import Om.Prim

data BasicPrim = PInt Int | PBool Bool
    deriving (Show, Eq)

instance PrimType BasicPrim Int where
    toPrim n           = PInt n
    fromPrim (PInt n)  = Just n
    fromPrim _         = Nothing

instance PrimType BasicPrim Bool where
    toPrim b           = PBool b
    fromPrim (PBool b) = Just b
    fromPrim _         = Nothing
