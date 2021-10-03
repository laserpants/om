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
import Om.Util (Name, Names, embed1, embed2, embed3)
import Text.Show.Deriving (deriveShow1)

data OmF p a
    = Var Name                           -- ^ Variable
    | Con Name                           -- ^ Data constructor
    | Lit p                              -- ^ Literal value
    | App [a]                            -- ^ Function application
    | Let Name a a                       -- ^ Let binding
    | If a a a                           -- ^ If-clause
    | Lam Name a                         -- ^ Lambda abstraction
    | Pat a [(Names, a)]                 -- ^ Pattern match expression

-- | Language expression
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

-- | Pattern matching wildcard symbol
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
