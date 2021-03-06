{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
module Om.Eval
  ( Value(..)
  , EvalEnv
  , Error(..)
  , EvalContext(..)
  , VarHook
  , ConHook
  , PatHook
  , Eval
  , EvalT
  , toString
  , runEval
  , runEvalT
  , primFun1
  , primFun2
  , primFun3
  , primFun4
  , primFun5
  , primValue
  , applyEvalEnv
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Text (unpack)
import Om.Lang
import Om.Prim
import Om.Util
import qualified Data.Map.Strict as Map

data Value p m
    = Value p
    | Data Name [Value p m]
    | Closure Name (m (Value p m)) (EvalEnv p m)
    | PrimFun (Fun p) [Value p m]

instance (Eq p) => Eq (Value p m) where
    (==) (Value p)   (Value q)   = p == q
    (==) (Data c vs) (Data d ws) = c == d && vs == ws
    (==) _ _                     = error "Not comparable"

toString :: (Show p) => Value p m -> String
toString = \case
    Value p      -> show p
    Data con []  -> unpack con
    Data con vs  -> unpack con <> flatten (toString <$> vs)
    Closure{}    -> "((function))"
    _            -> "((internal))"
  where
    flatten strs = "(" <> foldr (<>) "" (intersperse ", " strs) <> ")"

type EvalEnv p m = Map Name (m (Value p m))

data Error
    = UnboundIdentifier Name
    | NonTruthyCondition
    | TypeMismatch
    | RuntimeError
    deriving (Show, Eq)

type VarHook p m = Name -> m (Maybe (Value p m))
type ConHook p m = Name -> m (Maybe (Value p m))
type PatHook p m = [(Names, m (Value p m))] -> m (Value p m) -> m (Maybe (Value p m))

data EvalContext p m = EvalContext
    { evalEnv :: EvalEnv p m
    , varHook :: VarHook p m
    , conHook :: ConHook p m
    , patHook :: PatHook p m
    }

newtype EvalT m p a = EvalT { unEvalT :: ReaderT (EvalContext p (EvalT m p)) (ExceptT Error m) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadFix
      , MonadError Error
      , MonadReader (EvalContext p (EvalT m p)) )

type Eval = EvalT Identity

runEvalT :: (Monad m) => EvalT m p a -> EvalContext p (EvalT m p) -> m (Either Error a)
runEvalT e = runExceptT . runReaderT (unEvalT e)

runEval :: Eval p a -> EvalContext p (Eval p) -> Either Error a
runEval e = runIdentity . runEvalT e

applyEvalEnv :: (EvalEnv p m -> EvalEnv p m) -> EvalContext p m -> EvalContext p m
applyEvalEnv f EvalContext{ evalEnv, .. } = EvalContext{ evalEnv = f evalEnv, .. }

primFun1 :: (Monad m) => (PrimType p a, PrimType p b) => (a -> b) -> m (Value p m)
primFun1 f = pure $ PrimFun (fun1 f) []

primFun2 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c) => (a -> b -> c) -> m (Value p m)
primFun2 f = pure $ PrimFun (fun2 f) []

primFun3 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d) => (a -> b -> c -> d) -> m (Value p m)
primFun3 f = pure $ PrimFun (fun3 f) []

primFun4 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e) => (a -> b -> c -> d -> e) -> m (Value p m)
primFun4 f = pure $ PrimFun (fun4 f) []

primFun5 :: (Monad m) => (PrimType p a, PrimType p b, PrimType p c, PrimType p d, PrimType p e, PrimType p f) => (a -> b -> c -> d -> e -> f) -> m (Value p m)
primFun5 f = pure $ PrimFun (fun5 f) []

primValue :: (Monad m) => p -> m (Value p m)
primValue p = pure (Value p)
