module Om.Lang where

import Data.Text (Text)
import Data.Functor.Foldable

type Name  = Text
type Names = [Name]

type OMatrix a = [(Names, a)]

--data OVar 
--    = PrimFun Name
--    | Name Name

data OmF p a
    = Var Name           
    | Lit p              
    | App [a]            
    | Let Name a a       
    | Lam Name a         
    | If  a a a          
    | Pat a (OMatrix a)  

type Om p = Fix (OmF p)

omVar :: Name -> Om p
omVar = embed1 Var
{-# INLINE omVar #-}

omLit :: p -> Om p
omLit = embed1 Lit
{-# INLINE omLit #-}

omApp :: [Om p] -> Om p
omApp = embed1 App
{-# INLINE omApp #-}

omLet :: Name -> Om p -> Om p -> Om p
omLet = embed3 Let
{-# INLINE omLet #-}

omLam :: Name -> Om p -> Om p
omLam = embed2 Lam
{-# INLINE omLam #-}

omIf :: Om p -> Om p -> Om p -> Om p
omIf = embed3 If
{-# INLINE omIf #-}

omPat :: Om p -> CMatrix Om p -> Om p
omPat = embed2 Pat
{-# INLINE omPat #-}


