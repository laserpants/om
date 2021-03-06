{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Om.Plug.Nats
  ( NatType(..)
  , natsPlugin
  ) where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Om.Eval
import Om.Plug
import Om.Prim
import Om.Util
import Prelude hiding (succ)
import qualified Data.Map.Strict as Map

newtype NatType = FromInteger Integer
    deriving (Eq, Show, Num)

natsPlugin :: (Monad m, PrimType p NatType) => Plugin p (EvalT m p)
natsPlugin = plugin Nothing (Just natsConHook) (Just natsPatHook)

natsConHook :: (Monad m, PrimType p NatType) => ConHook p (EvalT m p)
natsConHook var

  | "succ" == var = do
      let closr body = pure (Closure "?0" body mempty)
      Just <$$> closr $ do
          env <- ask <#> evalEnv
          fromJust (Map.lookup "?0" env) >>= \case
              Value n ->
                  pure (Value (succ (fromJust (fromPrim n))))

  | "zero" == var =
      pure (Just (Value zero))

  | otherwise = pure Nothing

natsPatHook :: (Monad m, PrimType p NatType) => PatHook p (EvalT m p)
natsPatHook ((ps, e):_) val =
    val >>= \case
        Value p ->
            case (ps, fromPrim p) of

                (["succ", m], Just (FromInteger n)) | n > 0 -> do
                    let fun = Map.insert m (pure (Value (toPrim (FromInteger (n - 1)))))
                    Just <$> local (applyEvalEnv fun) e

                (["zero"], Just (FromInteger n)) | 0 == n -> Just <$> e

                _ -> pure Nothing

        _ -> pure Nothing

zero :: PrimType p NatType => p
zero = toPrim (FromInteger 0)

succ :: PrimType p NatType => NatType -> p
succ n = toPrim (n + FromInteger 1)
