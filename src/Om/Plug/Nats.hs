{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Om.Plug.Nats where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first, fst3, snd3, thd3, first3, second3, third3)
import Om.Eval
import Om.Plug
import Om.Prim
import Om.Util
import Prelude hiding (succ)
import qualified Data.Map.Strict as Map

newtype NatType = FromInteger Integer
    deriving (Eq, Show, Num)

zero :: PrimType p NatType => p
zero = toPrim (FromInteger 0)

succ :: PrimType p NatType => NatType -> p
succ n = toPrim (n + FromInteger 1)

natsPlugin :: (PrimType p NatType) => Plugin p (Eval p)
natsPlugin = plugin (Just natsLookupHook) Nothing

natsLookupHook :: (PrimType p NatType) => LookupHook p (Eval p)
natsLookupHook var

  | "succ" == var = do
      let closr body = pure (Closure "?0" body mempty)
      Just <$$> closr $ do
          env <- ask <#> fst3
          fromJust (Map.lookup "?0" env) >>= \case
              Value n -> 
                  pure (Value (succ (fromJust (fromPrim n))))

  | "zero" == var =
      pure (Just (Value zero))

  | otherwise = pure Nothing
