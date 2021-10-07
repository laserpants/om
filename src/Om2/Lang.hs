{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE StandaloneDeriving    #-}
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

omVar :: Name -> Om2
omVar = embed1 Var

omLit :: Prim -> Om2
omLit = embed1 Lit

omOp2 :: Op -> Om2 -> Om2 -> Om2
omOp2 = embed3 Op2

omDef :: Name -> [Name] -> Om2 -> Om2
omDef = embed3 Def

omExt :: Name -> [Om2] -> Om2
omExt = embed2 Ext
