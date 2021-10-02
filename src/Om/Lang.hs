{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Om.Lang
  ( Om
  , OmF(..)
  , omVar
  , omCon
  , omPrim
  , omLit
  , omApp
  , omLet
  , omIf
  , omLam
  , omPat
  , wcard
  , omData
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix(..))
import Data.Functor.Foldable
import Data.Ord.Deriving (deriveOrd1)
import Om.Util
import Text.Show.Deriving (deriveShow1)

data OmF p a
    = Var Name
    | Con Name
    | Lit p
    | App [a]
    | Let Name a a
    | If a a a
    | Lam Name a
    | Pat a [(Names, a)]

type Om p = Fix (OmF p)

omVar :: Name -> Om p
omVar = embed1 Var
{-# INLINE omVar #-}

omCon :: Name -> Om p
omCon = embed1 Con
{-# INLINE omCon #-}

omPrim :: Name -> Om p
omPrim = omVar . ("$" <>)
{-# INLINE omPrim #-}

omLit :: p -> Om p
omLit = embed1 Lit
{-# INLINE omLit #-}

omApp :: [Om p] -> Om p
omApp = embed1 App
{-# INLINE omApp #-}

omLet :: Name -> Om p -> Om p -> Om p
omLet = embed3 Let
{-# INLINE omLet #-}

omIf :: Om p -> Om p -> Om p -> Om p
omIf = embed3 If
{-# INLINE omIf #-}

omLam :: Name -> Om p -> Om p
omLam = embed2 Lam
{-# INLINE omLam #-}

omPat :: Om p -> [(Names, Om p)] -> Om p
omPat = embed2 Pat
{-# INLINE omPat #-}

wcard :: Name
wcard = "$_"
{-# INLINE wcard #-}

omData :: Name -> [Om p] -> Om p
omData con []   = omCon con
omData con args = omApp (omCon con:args)

deriving instance (Show p, Show a) => Show (OmF p a)
deriving instance (Eq   p, Eq   a) => Eq   (OmF p a)
deriving instance (Ord  p, Ord  a) => Ord  (OmF p a)

deriveShow1 ''OmF
deriveEq1   ''OmF
deriveOrd1  ''OmF

deriving instance Functor     (OmF p)
deriving instance Foldable    (OmF p)
deriving instance Traversable (OmF p)
