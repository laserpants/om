-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE StrictData            #-}
module Om2.Lang where

import Data.Fix (Fix(..))
import Data.Functor.Foldable
import Om.Util

data Prim
    = PInt Int
    | PBool Bool
    deriving (Show, Eq)

data Om2F a
    = Var Name
    | Lit Prim
    | Op2 Op a a 
    | Def Name [Name] a
    | Ext Name [a]
    deriving (Show, Eq)

data Op
    = Plus
    | Minus
    deriving (Show, Eq)


type Om2 = Fix Om2F

deriving instance Functor     Om2F
deriving instance Foldable    Om2F
deriving instance Traversable Om2F

