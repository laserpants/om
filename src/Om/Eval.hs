{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StrictData                 #-}
module Om.Eval where

import Control.Monad.Except
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

toString :: (Show p) => Value p m -> String
toString = \case
    Value p      -> show p
    Data con vs  -> unpack con <> " " <> flatten (toString <$> vs)
    Closure{}    -> "((function))"
    _            -> "((internal))"
  where
    flatten strs = "[" <> foldr (<>) "" (intersperse ", " strs) <> "]"

type EvalEnv p m = Map Name (m (Value p m))

data Error
    = UnboundIdentifier Name
    | NonTruthyCondition
    | TypeMismatch
    | RuntimeError
    deriving (Show, Eq)

type Hooks p m = Name -> m (Maybe (Value p m))

newtype Eval p a = Eval { unEval :: ReaderT (EvalEnv p (Eval p), Hooks p (Eval p)) (Either Error) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadError Error
      , MonadReader (EvalEnv p (Eval p), Hooks p (Eval p)) )

runEval :: Eval p a -> (EvalEnv p (Eval p), Hooks p (Eval p)) -> Either Error a
runEval = runReaderT . unEval

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
